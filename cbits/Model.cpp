#include "Model.h"

#include <QtCore/QMultiHash>

HsQMLAutoListModel::HsQMLAutoListModel(QObject* parent)
    : QAbstractListModel(parent)
    , mMode(ByReset)
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
    case ByReset:
        updateModelByReset(); break;
    case ByIndex:
        updateModelByIndex(); break;
    case ByKey:
        updateModelByKey(); break;
    }
}

void HsQMLAutoListModel::updateModelByReset()
{
    int srcLen = sourceLength();

    beginResetModel();
    mModel.clear();
    mModel.reserve(srcLen);
    for (int i=0; i<srcLen; i++) {
        mModel.append(Element(mSource.property(i)));
    }
    endResetModel();
}

void HsQMLAutoListModel::updateModelByIndex()
{
    int srcLen = sourceLength();
    int mdlLen = mModel.size();

    // Notify views which elements have changed
    for (int i=0; i<qMin(srcLen, mdlLen); i++) {
        handleInequality(mSource.property(i), i);
    }

    // Add or remove elements to/from the end of the list
    if (srcLen > mdlLen) {
        mModel.reserve(srcLen);
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
    int srcLen = sourceLength();

    // Build a map of element key's highest indices in the new source
    typedef QHash<QString, int> SrcDict;
    SrcDict srcDict;
    for (int i=0; i<srcLen; i++) {
        QJSValue srcVal = mSource.property(i);
        srcDict.insert(keyFunction(srcVal), i);
    }

    // Build a map of element key's previous indices in the old model
    typedef QMultiHash<QString, Element*> ModelDict;
    ModelDict modelDict;
    for (int idx = mModel.size()-1; idx >= 0; --idx) {
        Element& e = mModel[idx];
        e.mKey = keyFunction(e.mValue);
        e.mIndex = idx;
        modelDict.insert(e.mKey, &e);
    }

    // Rearrange and insert new elements
    mModel.reserve(srcLen);
    for (int i=0; i<srcLen; i++) {
        QJSValue srcVal = mSource.property(i);
        QString srcKey = keyFunction(srcVal);

        ModelDict::iterator it = modelDict.find(srcKey);
        if (it != modelDict.end()) {
            Element& e = **it;
            Q_ASSERT(e.mIndex >= i);
            modelDict.erase(it);

            // Try removing elements before target if possible
            while (e.mIndex > i) {
                const Element& old = mModel[i];
                SrcDict::iterator srcIt = srcDict.find(old.mKey);
                if (srcIt != srcDict.end() && i <= srcIt.value()) {
                    // Old element is still needed by the new source
                    Q_ASSERT(i != srcIt.value());
                    break;
                }

                beginRemoveRows(QModelIndex(), i, i);
                mModel.removeAt(i);
                endRemoveRows();
                e.mIndex--;
            }

            // Move target element earlier in list if needed
            if (e.mIndex > i) {
                beginMoveRows(QModelIndex(), e.mIndex, e.mIndex,
                    QModelIndex(), i);
                mModel.move(e.mIndex, i);
                endMoveRows();
            }

            // Has value changed?
            handleInequality(srcVal, i);
        }
        else {
            beginInsertRows(QModelIndex(), i, i);
            mModel.insert(i, Element(srcVal));
            endInsertRows();
        }

        // Renumber remaining old elements
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

int HsQMLAutoListModel::sourceLength()
{
    return mSource.property("length").toInt();
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
