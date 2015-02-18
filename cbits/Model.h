#ifndef HSQML_MODEL_H
#define HSQML_MODEL_H

#include <QtCore/QAbstractListModel>
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
    Q_PROPERTY(QJSValue identityTest READ identityTest WRITE setIdentityTest)
    Q_PROPERTY(QJSValue keyFunction READ keyFunction WRITE setKeyFunction)

public:
    enum Mode {
        ByIndex,
        ByEquality,
        ByIdentity,
        ByKey
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
    void setIdentityTest(const QJSValue&);
    QJSValue identityTest() const;
    void setKeyFunction(const QJSValue&);
    QJSValue keyFunction() const;

private:
    struct Element {
        Element(const QJSValue& value) : mValue(value), mIndex(0) {}
        QJSValue mValue;
        int mIndex;
    };

    void updateModel();
    bool modeTest(const QJSValue&, const QString&, int, int);
    void handleInequality(const QJSValue&, int);
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
    typedef QList<Element> Model;
    Model mModel;
    bool mDefer;
    bool mPending;
};

#endif //HSQML_MODEL_H
