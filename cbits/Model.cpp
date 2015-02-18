#include "Model.h"

#include <QtCore/QMultiHash>

HsQMLAutoListModel::HsQMLAutoListModel(QObject* parent)
    : QAbstractListModel(parent)
    , mMode(ByIndex)
    , mEqualityTestValid(false)
    , mIdentityTestValid(false)
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

void HsQMLAutoListModel::setIdentityTest(const QJSValue& identityTest)
{
    mIdentityTest = identityTest;
    mIdentityTestValid = identityTest.isCallable();
    updateModel();
}

QJSValue HsQMLAutoListModel::identityTest() const
{
    return mIdentityTest;
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

    typedef QMultiHash<QString, Element*> Dict;
    Dict dict;
    if (mMode == ByKey) {
        for (int idx = mModel.size()-1; idx >= 0; --idx) {
            Element& e = mModel[idx];
            dict.insert(keyFunction(e.mValue), &e);
            e.mIndex = idx;
        }
    }

    int srcLen = mSource.property("length").toInt();
    for (int i=0; i<srcLen; i++) {
        QJSValue srcVal = mSource.property(i);
        QString srcKey = mMode == ByKey ? keyFunction(srcVal) : "";
        if (modeTest(srcVal, srcKey, i, i)) {
            if (mMode == ByKey) {
                Element* e = dict.take(srcKey);
            }
            handleInequality(srcVal, i);
        }
        else if (mMode == ByKey) {
            Dict::iterator it = dict.find(srcKey);
            if (it != dict.end()) {
                const Element& e = **it;
                Q_ASSERT(e.mIndex > i);
                dict.erase(it);
                beginMoveRows(QModelIndex(), e.mIndex, e.mIndex,
                    QModelIndex(), i);
                mModel.move(e.mIndex, i);
                endMoveRows();
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
        else {
            bool matched = false;
            for (int j=i+1; j<mModel.size(); j++) {
                if (modeTest(srcVal, srcKey, i, j)) {
                    beginRemoveRows(QModelIndex(), i, j-1);
                    mModel.erase(mModel.begin()+i, mModel.begin()+j);
                    endRemoveRows();
                    handleInequality(srcVal, i);
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                beginInsertRows(QModelIndex(), i, i);
                mModel.insert(i, Element(srcVal));
                endInsertRows();
            }
        }
    }
    if (mModel.size() > srcLen) {
        beginRemoveRows(QModelIndex(), srcLen, mModel.size()-1);
        mModel.erase(mModel.begin()+srcLen, mModel.end());
        endRemoveRows();
    }
}

bool HsQMLAutoListModel::modeTest(
    const QJSValue& a, const QString& aKey, int aIdx, int bIdx)
{
    if (bIdx >= mModel.size()) {
        return false;
    }
    const QJSValue& b = mModel[bIdx].mValue;
    switch (mMode) {
    case ByIndex: default: return aIdx == bIdx;
    case ByEquality: return equalityTest(a, b);
    case ByIdentity: return identityTest(a, b);
    case ByKey: return aKey == keyFunction(b);
    }
}

void HsQMLAutoListModel::handleInequality(
    const QJSValue& a, int i)
{
    if ((mMode != ByEquality) && !equalityTest(a, mModel[i].mValue)) {
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

bool HsQMLAutoListModel::identityTest(const QJSValue& a, const QJSValue& b)
{
    if (mIdentityTestValid) {
        QJSValueList args;
        args.append(a);
        args.append(b);
        return mIdentityTest.call(args).toBool();
    }
    else {
        return a.strictlyEquals(b);
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
