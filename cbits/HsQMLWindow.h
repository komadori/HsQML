#ifndef HSQML_WINDOW_H
#define HSQML_WINDOW_H

#include <QMainWindow>
#include <QDeclarativeContext>
#include <QDeclarativeItem>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QUrl>

#include "HsQMLManager.h"

class QDeclarativeComponent;

class HsQMLWindow : public QObject
{
    Q_OBJECT

public:
    HsQMLWindow(HsQMLEngine*);
    virtual ~HsQMLWindow();
    QUrl source() const;
    void setSource(const QUrl&);
    Q_PROPERTY(QUrl source READ source WRITE setSource);
    QString title() const;
    void setTitle(const QString&);
    Q_PROPERTY(QString title READ title WRITE setTitle);
    bool visible() const;
    void setVisible(bool);
    Q_PROPERTY(bool visible READ visible WRITE setVisible);
    Q_SCRIPTABLE void close();

private:
    Q_SLOT void completeSetSource();
    Q_SLOT void completeClose();
    HsQMLEngine* mEngine;
    QDeclarativeContext mContext;
    QMainWindow mWindow;
    QGraphicsScene mScene;
    QGraphicsView mView;
    QUrl mSource;
    QDeclarativeComponent* mComponent;
};

#endif /*HSQML_WINDOW_H*/
