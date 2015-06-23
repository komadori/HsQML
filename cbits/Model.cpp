#include "Model.h"

#include <QtCore/QMultiHash>

HsQMLAutoListModel::HsQMLAutoListModel(QObject* parent)
    : QAbstractListModel(parent)
    , mMode(ByIndex)
    , mEqualityTestValid(false)
    , mKeyFunctionValid(false)
    , mDefer(false)
    , mPending(false)
{
}

void HsQMLAutoListModel::classBegin()
{
    mDefer = true;
}

void HsQMLAutoListModel::componentComplete()
{
    mDefer = false;
    if (mPending) {
        updateModel();
    }
}

int HsQMLAutoListModel::rowCount(const QModelIndex&) const
{
    return mModel.size();
}

QVariant HsQMLAutoListModel::data(const QModelIndex& index, int) const
{
    return mModel[index.row()].mValue.toVariant();
}

QHash<int, QByteArray> HsQMLAutoListModel::roleNames() const
{
    QHash<int, QByteArray> roleNames;
    roleNames.insert(Qt::UserRole, QByteArray("modelData"));
    return roleNames;
}

void HsQMLAutoListModel::setMode(Mode mode)
{
    mMode = mode;
}

HsQMLAutoListModel::Mode HsQMLAutoListModel::mode() const
{
    return mMode;
}

void HsQMLAutoListModel::setSource(const QJSValue& source)
{
    mSource = source;
    updateModel();
}

QJSValue HsQMLAutoListModel::source() const
{
    return mSource;
}

void HsQMLAutoListModel::setEqualityTest(const QJSValue& equalityTest)
{
    mEqualityTest = equalityTest;
    mEqualityTestValid = equalityTest.isCallable();
    updateModel();
}

QJSValue HsQMLAutoListModel::equalityTest() const
{
    return mEqualityTest;
}

void HsQMLAutoListModel::setKeyFunction(const QJSValue& keyFunction)
{
    mKeyFunction = keyFunction;
    mKeyFunctionValid = keyFunction.isCallable();
    updateModel();
}

QJSValue HsQMLAutoListModel::keyFunction() const
{
    return mKeyFunction;
}

void HsQMLAutoListModel::updateModel()
{
    if (mDefer) {
        mPending = true;
        return;
    }

    switch (mMode) {
    case ByIndex:
        updateModelByIndex(); break;
    case ByKey:
        updateModelByKey(); break;
    }
}

void HsQMLAutoListModel::updateModelByIndex()
{
    int srcLen = mSource.property("length").toInt();
    int mdlLen = mModel.size();

    // Notify views which elements have changed
    for (int i=0; i<qMin(srcLen, mdlLen); i++) {
        handleInequality(mSource.property(i), i);
    }

    // Add or remove elements to/from the end of the list
    if (srcLen > mdlLen) {
        beginInsertRows(QModelIndex(), srcLen, mModel.size()-1);
        for (int i=mdlLen; i<srcLen; i++) {
            mModel.append(Element(mSource.property(i)));
        }
        endInsertRows();
    }
    else if (mdlLen > srcLen) {
        beginRemoveRows(QModelIndex(), srcLen, mModel.size()-1);
        mModel.erase(mModel.begin()+srcLen, mModel.end());
        endRemoveRows();
    }
}

void HsQMLAutoListModel::updateModelByKey()
{
    // Build a map of previous element indices
    typedef QMultiHash<QString, Element*> Dict;
    Dict dict;
    for (int idx = mModel.size()-1; idx >= 0; --idx) {
        Element& e = mModel[idx];
        dict.insert(keyFunction(e.mValue), &e);
        e.mIndex = idx;
    }

    // Rearrange and insert new elements
    int srcLen = mSource.property("length").toInt();
    for (int i=0; i<srcLen; i++) {
        QJSValue srcVal = mSource.property(i);
        QString srcKey = keyFunction(srcVal);

        Dict::iterator it = dict.find(srcKey);
        if (it != dict.end()) {
            const Element& e = **it;
            Q_ASSERT(e.mIndex >= i);
            dict.erase(it);
            if (e.mIndex > i) {
                beginMoveRows(QModelIndex(), e.mIndex, e.mIndex,
                    QModelIndex(), i);
                mModel.move(e.mIndex, i);
                endMoveRows();
            }
            handleInequality(srcVal, i);
        }
        else {
            beginInsertRows(QModelIndex(), i, i);
            mModel.insert(i, Element(srcVal));
            endInsertRows();
        }
        for (int j=i; j<mModel.size(); j++) {
            mModel[j].mIndex = j;
        }
    }

    // Remove excess elements from the end of the list
    if (mModel.size() > srcLen) {
        beginRemoveRows(QModelIndex(), srcLen, mModel.size()-1);
        mModel.erase(mModel.begin()+srcLen, mModel.end());
        endRemoveRows();
    }
}

void HsQMLAutoListModel::handleInequality(
    const QJSValue& a, int i)
{
    if (!equalityTest(a, mModel[i].mValue)) {
        mModel[i].mValue = a;
        QModelIndex idx = createIndex(i, 0);
        dataChanged(idx, idx);
    }
}

bool HsQMLAutoListModel::equalityTest(const QJSValue& a, const QJSValue& b)
{
    if (mEqualityTestValid) {
        QJSValueList args;
        args.append(a);
        args.append(b);
        return mEqualityTest.call(args).toBool();
    }
    else {
        return a.equals(b);
    }
}

QString HsQMLAutoListModel::keyFunction(const QJSValue& a)
{
    if (mKeyFunctionValid) {
        QJSValueList args;
        args.append(a);
        return mKeyFunction.call(args).toString();
    }
    else {
        return a.toString();
    }
}
