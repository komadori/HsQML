#ifndef HSQML_CANVAS_H
#define HSQML_CANVAS_H

#include <QtCore/QObject>
#include <QtCore/QExplicitlySharedDataPointer>
#include <QtCore/QMetaType>
#include <QtCore/QScopedPointer>
#include <QtCore/QSharedData>
#include <QtGui/QOpenGLFramebufferObject>
#include <QtQuick/QQuickItem>

#include "hsqml.h"

class QSGTexture;
class QQuickWindow;
class HsQMLCanvasBackEnd;

class HsQMLGLContextData : public QSharedData
{
public:
    HsQMLGLContextData(HsQMLGLSyncCb, HsQMLGLPaintCb);
    ~HsQMLGLContextData();

    HsQMLGLSyncCb mSyncCb;
    HsQMLGLPaintCb mPaintCb;
};

class HsQMLGLDelegateImpl : public QSharedData
{
public:
    HsQMLGLDelegateImpl(HsQMLGLMakeContextCb);
    ~HsQMLGLDelegateImpl();

    HsQMLGLMakeContextCb mMakeContextCb;
};

class HsQMLGLDelegate
{
public:
    HsQMLGLDelegate();
    ~HsQMLGLDelegate();
    void setup(HsQMLGLMakeContextCb);
    typedef QExplicitlySharedDataPointer<HsQMLGLContextData> ContextDataRef;
    ContextDataRef makeContextData();

private:
    QExplicitlySharedDataPointer<HsQMLGLDelegateImpl> mImpl;
};

Q_DECLARE_METATYPE(HsQMLGLDelegate)

class HsQMLCanvas : public QQuickItem
{
    Q_OBJECT
    Q_ENUMS(DisplayMode)
    Q_PROPERTY(DisplayMode displayMode READ displayMode WRITE setDisplayMode
        NOTIFY displayModeChanged)
    Q_PROPERTY(qreal canvasWidth READ canvasWidth WRITE setCanvasWidth
        RESET unsetCanvasWidth NOTIFY canvasWidthChanged)
    Q_PROPERTY(qreal canvasHeight READ canvasHeight WRITE setCanvasHeight
        RESET unsetCanvasHeight NOTIFY canvasHeightChanged)
    Q_PROPERTY(QVariant delegate READ delegate WRITE setDelegate
        NOTIFY delegateChanged)

public:
    enum DisplayMode {Above, Below, Inline};

    HsQMLCanvas(QQuickItem* = NULL);
    ~HsQMLCanvas();

private:
    Q_DISABLE_COPY(HsQMLCanvas);

    void geometryChanged(const QRectF&, const QRectF&) Q_DECL_OVERRIDE;
    QSGNode* updatePaintNode(QSGNode*, UpdatePaintNodeData*) Q_DECL_OVERRIDE;
    void detachBackEnd();
    DisplayMode displayMode() const;
    void setDisplayMode(DisplayMode);
    qreal canvasWidth() const;
    void setCanvasWidth(qreal, bool = true);
    void unsetCanvasWidth();
    qreal canvasHeight() const;
    void setCanvasHeight(qreal, bool = true);
    void unsetCanvasHeight();
    QVariant delegate() const;
    void setDelegate(const QVariant&);
    Q_SIGNAL void displayModeChanged();
    Q_SIGNAL void canvasWidthChanged();
    Q_SIGNAL void canvasHeightChanged();
    Q_SIGNAL void delegateChanged();
    Q_SLOT void doWindowChanged(QQuickWindow*);

    QQuickWindow* mWindow;
    HsQMLCanvasBackEnd* mBackEnd;
    DisplayMode mDisplayMode;
    qreal mCanvasWidth;
    bool mCanvasWidthSet;
    qreal mCanvasHeight;
    bool mCanvasHeightSet;
    QVariant mDelegate;
    HsQMLGLDelegate::ContextDataRef mGLContextData;
};

class HsQMLCanvasBackEnd : public QObject
{
    Q_OBJECT

public:
    HsQMLCanvasBackEnd(QQuickWindow*);
    ~HsQMLCanvasBackEnd();
    void setGLContextData(const HsQMLGLDelegate::ContextDataRef&);
    void setModeSize(HsQMLCanvas::DisplayMode, qreal, qreal);
    QSGTexture* texture() const;

private:
    Q_DISABLE_COPY(HsQMLCanvasBackEnd)

    Q_SLOT void doRendering();

    QQuickWindow* mWindow;
    HsQMLGLDelegate::ContextDataRef mGLContextData;
    HsQMLCanvas::DisplayMode mDisplayMode;
    qreal mCanvasWidth;
    qreal mCanvasHeight;
    QScopedPointer<QOpenGLFramebufferObject> mFBO;
    QScopedPointer<QSGTexture> mTexture;
};

#endif //HSQML_CANVAS_H
