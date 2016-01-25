#include <QtCore/QMultiHash>

#include "Model.h"
#include "Manager.h"

HsQMLAutoListModel::HsQMLAutoListModel(QObject* parent)
    : QAbstractListModel(parent)
    , mMode(ByReset)
    , mEqualityTestValid(false)
    , mKeyFunctionValid(false)
    , mOldOffset(0)
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
    return fromOldIndex(mOldModel.size());
}

QVariant HsQMLAutoListModel::data(const QModelIndex& index, int) const
{
    int i = index.row();
    const Element& e =
        i < mNewModel.size() ? mNewModel[i] : mOldModel[toOldIndex(i)];
    return e.mValue.toVariant();
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
    bool change = !mSource.strictlyEquals(source);
    mSource = source;
    if (change) {
        sourceChanged();
    }
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
}

QJSValue HsQMLAutoListModel::equalityTest() const
{
    return mEqualityTest;
}

void HsQMLAutoListModel::setKeyFunction(const QJSValue& keyFunction)
{
    mKeyFunction = keyFunction;
    mKeyFunctionValid = keyFunction.isCallable();
    mRehash = true;
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
        updateModelByKey(true); break;
    case ByKeyNoReorder:
        updateModelByKey(false); break;
    }
}

void HsQMLAutoListModel::updateModelByReset()
{
    int srcLen = sourceLength();

    HSQML_LOG(3, QString().sprintf(
        "AutoListModel.ByReset: Inserted %d elements.", srcLen));
    beginResetModel();
    mOldModel.clear();
    mOldModel.reserve(srcLen);
    for (int i=0; i<srcLen; i++) {
        mOldModel.append(Element(mSource.property(i)));
    }
    endResetModel();

    // This mode doesn't maintain the hashes
    mRehash = true;
}

void HsQMLAutoListModel::updateModelByIndex()
{
    int srcLen = sourceLength();
    int oldLen = mOldModel.size();

    // Notify views which elements have changed
    for (int i=0; i<qMin(srcLen, oldLen); i++) {
        handleInequality(mSource.property(i), mOldModel, i);
    }

    // Add or remove elements to/from the end of the list
    if (srcLen > oldLen) {
        mOldModel.reserve(srcLen);
        HSQML_LOG(3, QString().sprintf(
            "AutoListModel.ByIndex: Inserted %d extra elements at %d.",
            srcLen - oldLen, oldLen));
        beginInsertRows(QModelIndex(), oldLen, srcLen-1);
        for (int i=oldLen; i<srcLen; i++) {
            mOldModel.append(Element(mSource.property(i)));
        }
        endInsertRows();
    }
    else if (oldLen > srcLen) {
        HSQML_LOG(3, QString().sprintf(
            "AutoListModel.ByIndex: Removed %d excess elements at %d.",
            oldLen - srcLen, srcLen));
        beginRemoveRows(QModelIndex(), srcLen, oldLen-1);
        mOldModel.erase(mOldModel.begin()+srcLen, mOldModel.end());
        endRemoveRows();
    }

    // This mode doesn't maintain the hashes
    mRehash = true;
}

void HsQMLAutoListModel::updateModelByKey(bool reorder)
{
    int srcLen = sourceLength();

    // Build a map of element key's highest indices in the new source
    typedef QHash<QString, int> SrcDict;
    SrcDict srcDict;
    if (reorder) {
        for (int i=0; i<srcLen; i++) {
            QJSValue srcVal = mSource.property(i);
            srcDict.insert(keyFunction(srcVal), i);
        }
    }

    // Build a map of element key's previous indices in the old model
    typedef QMultiHash<QString, int> ModelDict;
    ModelDict modelDict;
    for (int idx = mOldModel.size()-1; idx >= 0; --idx) {
        Element& e = mOldModel[idx];
        if (mRehash) {
            e.mKey = keyFunction(e.mValue);
        }
        modelDict.insert(e.mKey, idx);
    }
    mRehash = false;

    // Rearrange and insert new elements
    Q_ASSERT(!mNewModel.size() && !mOldOffset);
    mNewModel.reserve(srcLen);
    for (int i=0; i<srcLen; i++) {
        QJSValue srcVal = mSource.property(i);
        QString srcKey = keyFunction(srcVal);

        ModelDict::iterator it = modelDict.find(srcKey);
        if (it != modelDict.end()) {
            const int elemIdx = *it;
            Q_ASSERT(elemIdx >= mOldOffset);
            Q_ASSERT(elemIdx < mOldModel.size());
            modelDict.erase(it);

            // Try removing elements before target
            while (elemIdx > mOldOffset) {
                const Element& nextElem = mOldModel[mOldOffset];

                if (reorder) {
                    // Check if element will be used later
                    SrcDict::iterator srcIt = srcDict.find(nextElem.mKey);
                    if (srcIt != srcDict.end()) {
                        if (srcIt.value() >= i) {
                            // Old element is still needed by the new source
                            break;
                        }
                    }
                }

                // Remove element
                HSQML_LOG(3, QString().sprintf(
                    "AutoListModel.ByKey: Removed element at %d.", i));
                beginRemoveRows(QModelIndex(), i, i);
                mOldOffset++;
                endRemoveRows();
                modelDict.erase(modelDict.find(nextElem.mKey));
            }

            // Move target element earlier in list if needed
            if (elemIdx > mOldOffset) {
                Q_ASSERT(reorder);
                int srcIdx = fromOldIndex(elemIdx);
                HSQML_LOG(3, QString().sprintf(
                    "AutoListModel.ByKey: Moved element at %d to %d.",
                    srcIdx, i));
                beginMoveRows(QModelIndex(), srcIdx, srcIdx, QModelIndex(), i);
                mNewModel.append(mOldModel[elemIdx]);
                mOldModel.remove(elemIdx);
                endMoveRows();

                // Renumber remaining indices in the old model
                for (ModelDict::iterator renumIt = modelDict.begin();
                     renumIt != modelDict.end();) {
                    ModelDict::iterator currIt = renumIt++;
                    Q_ASSERT(currIt.value() >= mOldOffset);
                    if (currIt.value() > elemIdx) {
                        currIt.value()--;
                    }
                }
            }
            else {
                // Transfer element from old to new model
                mNewModel.append(mOldModel[elemIdx]);
                mOldOffset++;
            }

            // Has value changed?
            handleInequality(srcVal, mNewModel, i);
            Q_ASSERT(mNewModel.size() == i+1);
        }
        else {
            HSQML_LOG(3, QString().sprintf(
                "AutoListModel.ByKey: Inserted element at %d.", i));
            beginInsertRows(QModelIndex(), i, i);
            mNewModel.append(Element(srcVal, srcKey));
            endInsertRows();
        }
    }

    // Move element to the old model, removing any excess elements from the end
    bool excess = mOldOffset < mOldModel.size();
    if (excess) {
        HSQML_LOG(3, QString().sprintf(
            "AutoListModel.ByKey: Removed %d excess elements at %d.",
            mOldModel.size() - mOldOffset, srcLen));
        beginRemoveRows(QModelIndex(), srcLen, rowCount(QModelIndex())-1);
    }
    mNewModel.swap(mOldModel);
    mNewModel.clear();
    mOldOffset = 0;
    if (excess) {
        endRemoveRows();
    }
}

int HsQMLAutoListModel::sourceLength()
{
    return mSource.property("length").toInt();
}

int HsQMLAutoListModel::toOldIndex(int i) const
{
    return i - mNewModel.size() + mOldOffset;
}

int HsQMLAutoListModel::fromOldIndex(int i) const
{
    return i + mNewModel.size() - mOldOffset;
}

void HsQMLAutoListModel::handleInequality(
    const QJSValue& a, Model& model, int i)
{
    if (!equalityTest(a, model[i].mValue)) {
        model[i].mValue = a;
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
