#ifndef HSQML_MODEL_H
#define HSQML_MODEL_H

#include <QtCore/QAbstractListModel>
#include <QtCore/QVector>
#include <QtQml/QJSValue>
#include <QtQml/QQmlParserStatus>

class HsQMLAutoListModel : public QAbstractListModel, public QQmlParserStatus
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)
    Q_ENUMS(Mode)
    Q_PROPERTY(Mode mode READ mode WRITE setMode)
    Q_PROPERTY(QJSValue source READ source WRITE setSource)
    Q_PROPERTY(QJSValue equalityTest READ equalityTest WRITE setEqualityTest)
    Q_PROPERTY(QJSValue keyFunction READ keyFunction WRITE setKeyFunction)

public:
    enum Mode {
        ByReset,
        ByIndex,
        ByKey,
        ByKeyNoReorder
    };

    HsQMLAutoListModel(QObject* = NULL);

    int rowCount(const QModelIndex&) const;
    QVariant data(const QModelIndex&, int) const;
    QHash<int, QByteArray> roleNames() const;

    void classBegin();
    void componentComplete();
    void setMode(Mode);
    Mode mode() const;
    void setSource(const QJSValue&);
    QJSValue source() const;
    void setEqualityTest(const QJSValue&);
    QJSValue equalityTest() const;
    void setKeyFunction(const QJSValue&);
    QJSValue keyFunction() const;

private:
    struct Element {
        Element() {}
        Element(const QJSValue& value) : mValue(value) {}
        Element(const QJSValue& value, const QString& key)
            : mValue(value), mKey(key) {}
        QJSValue mValue;
        QString mKey;
    };
    typedef QVector<Element> Model;

    void updateModel();
    void updateModelByReset();
    void updateModelByIndex();
    void updateModelByKey(bool);
    int sourceLength();
    int toOldIndex(int) const;
    int fromOldIndex(int) const;
    bool modeTest(const QJSValue&, const QString&, int, int);
    void handleInequality(const QJSValue&, Model&, int);
    bool equalityTest(const QJSValue&, const QJSValue&);
    bool identityTest(const QJSValue&, const QJSValue&);
    QString keyFunction(const QJSValue&);

    Mode mMode;
    QJSValue mSource;
    QJSValue mEqualityTest;
    bool mEqualityTestValid;
    QJSValue mIdentityTest;
    bool mIdentityTestValid;
    QJSValue mKeyFunction;
    bool mKeyFunctionValid;
    Model mOldModel;
    Model mNewModel;
    int mOldOffset;
    bool mDefer;
    bool mPending;
    bool mRehash;
};

#endif //HSQML_MODEL_H
