#ifndef HSQML_CANVAS_H
#define HSQML_CANVAS_H

#include <QtCore/QObject>
#include <QtCore/QExplicitlySharedDataPointer>
#include <QtCore/QMetaType>
#include <QtCore/QScopedPointer>
#include <QtCore/QSharedData>
#include <QtGui/qopengl.h>
#include <QtGui/QOpenGLContext>
#include <QtGui/QOpenGLFramebufferObject>
#include <QtQuick/QQuickItem>

#include "hsqml.h"

class QSGTexture;
class QSGTransformNode;
class QQuickWindow;
class HsQMLCanvasBackEnd;
class HsQMLWindowInfoImpl;

class HsQMLGLCallbacks : public QSharedData
{
public:
    HsQMLGLCallbacks(
        HsQMLGLSetupCb, HsQMLGLCleanupCb, HsQMLGLSyncCb, HsQMLGLPaintCb);
    ~HsQMLGLCallbacks();

    HsQMLGLSetupCb mSetupCb;
    HsQMLGLCleanupCb mCleanupCb;
    HsQMLGLSyncCb mSyncCb;
    HsQMLGLPaintCb mPaintCb;
};

class HsQMLGLDelegateImpl : public QSharedData
{
public:
    HsQMLGLDelegateImpl(HsQMLGLMakeCallbacksCb);
    ~HsQMLGLDelegateImpl();

    HsQMLGLMakeCallbacksCb mMakeCallbacksCb;
};

class HsQMLGLDelegate
{
public:
    HsQMLGLDelegate();
    ~HsQMLGLDelegate();
    void setup(HsQMLGLMakeCallbacksCb);
    typedef QExplicitlySharedDataPointer<HsQMLGLCallbacks> CallbacksRef;
    CallbacksRef makeCallbacks();

private:
    QExplicitlySharedDataPointer<HsQMLGLDelegateImpl> mImpl;
};

Q_DECLARE_METATYPE(HsQMLGLDelegate)

class HsQMLWindowInfoImpl : public QSharedData
{
public:
    HsQMLWindowInfoImpl(QQuickWindow*);
    
private:
    friend class HsQMLWindowInfo;
    QQuickWindow* mWin;
    int mBelowCount;
    bool mBelowClear;
};

class HsQMLWindowInfo
{
public:
    HsQMLWindowInfo();
    static HsQMLWindowInfo getWindowInfo(QQuickWindow*);
    void addBelow();
    void removeBelow();
    bool needsBelowClear();
    void endFrame();

private:
    QExplicitlySharedDataPointer<HsQMLWindowInfoImpl> mImpl;
};

Q_DECLARE_METATYPE(HsQMLWindowInfo)

class HsQMLCanvas : public QQuickItem
{
    Q_OBJECT
    Q_ENUMS(Status)
    Q_ENUMS(DisplayMode)
    Q_PROPERTY(DisplayMode displayMode READ displayMode WRITE setDisplayMode
        NOTIFY displayModeChanged)
    Q_PROPERTY(qreal canvasWidth READ canvasWidth WRITE setCanvasWidth
        RESET unsetCanvasWidth NOTIFY canvasWidthChanged)
    Q_PROPERTY(qreal canvasHeight READ canvasHeight WRITE setCanvasHeight
        RESET unsetCanvasHeight NOTIFY canvasHeightChanged)
    Q_PROPERTY(QVariant delegate READ delegate WRITE setDelegate
        NOTIFY delegateChanged)
    Q_PROPERTY(QJSValue model READ model WRITE setModel
        NOTIFY modelChanged)
    Q_PROPERTY(Status status READ status NOTIFY statusChanged)

public:
    enum Status {Okay, BadDelegate, BadModel, BadConfig, BadProcs, BadBind};
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
    QJSValue model() const;
    void setModel(const QJSValue&);
    Status status() const;
    void setStatus(Status, bool = false);
    Q_SIGNAL void displayModeChanged();
    Q_SIGNAL void canvasWidthChanged();
    Q_SIGNAL void canvasHeightChanged();
    Q_SIGNAL void delegateChanged();
    Q_SIGNAL void modelChanged();
    Q_SIGNAL void statusChanged();
    Q_SLOT void doWindowChanged(QQuickWindow*);
    Q_SLOT void doBackEndStatusChanged(HsQMLCanvas::Status);

    QQuickWindow* mWindow;
    HsQMLCanvasBackEnd* mBackEnd;
    Status mStatus;
    Status mFrontStatus;
    Status mBackStatus;
    DisplayMode mDisplayMode;
    qreal mCanvasWidth;
    bool mCanvasWidthSet;
    qreal mCanvasHeight;
    bool mCanvasHeightSet;
    QVariant mDelegate;
    HsQMLGLDelegate::CallbacksRef mGLCallbacks;
    QJSValue mModel;
    bool mLoadModel;
    bool mValidModel;
};

class HsQMLCanvasBackEnd : public QObject
{
    Q_OBJECT

public:
    HsQMLCanvasBackEnd(
        QQuickWindow*,
        const HsQMLGLDelegate::CallbacksRef&,
        HsQMLCanvas::DisplayMode);
    ~HsQMLCanvasBackEnd();
    void setTransformNode(QSGTransformNode*, qreal, qreal);
    QSGTexture* updateFBO(qreal, qreal);
    HsQMLCanvas::Status status() const;

private:
    Q_DISABLE_COPY(HsQMLCanvasBackEnd)

    void setStatus(HsQMLCanvas::Status);
    Q_SLOT void doRendering();
    Q_SLOT void doEndFrame();
    Q_SLOT void doCleanup();
    Q_SLOT void doCleanupKill();
    Q_SIGNAL void statusChanged(HsQMLCanvas::Status);

    typedef void (*GLViewportFn)(GLint,GLint,GLsizei,GLsizei);
    typedef void (*GLClearColorFn)(GLclampf,GLclampf,GLclampf,GLclampf);
    typedef void (*GLClearFn)(GLbitfield);

    QQuickWindow* mWindow;
    HsQMLWindowInfo mWinInfo;
    HsQMLGLDelegate::CallbacksRef mGLCallbacks;
    QOpenGLContext* mGL;
    GLViewportFn mGLViewportFn;
    GLClearColorFn mGLClearColorFn;
    GLClearFn mGLClearFn;
    HsQMLCanvas::Status mStatus;
    HsQMLCanvas::DisplayMode mDisplayMode;
    qreal mItemWidth;
    qreal mItemHeight;
    QSGTransformNode* mTransformNode;
    qreal mCanvasWidth;
    qreal mCanvasHeight;
    QScopedPointer<QOpenGLFramebufferObject> mFBO;
    QScopedPointer<QSGTexture> mTexture;
};

class HsQMLContextControl : public QQuickItem
{
    Q_OBJECT
    Q_ENUMS(ContextType)
    Q_ENUMS(ContextProfile)
    Q_PROPERTY(int majorVersion READ majorVersion WRITE setMajorVersion
        RESET unsetMajorVersion NOTIFY contextChanged);
    Q_PROPERTY(int minorVersion READ minorVersion WRITE setMinorVersion
        RESET unsetMinorVersion NOTIFY contextChanged);
    Q_PROPERTY(ContextType contextType READ contextType WRITE setContextType
        RESET unsetContextType NOTIFY contextChanged);
    Q_PROPERTY(ContextProfile contextProfile READ contextProfile
        WRITE setContextProfile RESET unsetContextProfile
        NOTIFY contextChanged);
    Q_PROPERTY(bool deprecatedFunctions READ deprecatedFunctions
        WRITE setDeprecatedFunctions
        RESET unsetDeprecatedFunctions NOTIFY contextChanged);
    Q_PROPERTY(int depthBufferSize READ depthBufferSize
        WRITE setDepthBufferSize
        RESET unsetDepthBufferSize NOTIFY contextChanged);
    Q_PROPERTY(int stencilBufferSize READ stencilBufferSize
        WRITE setStencilBufferSize
        RESET unsetStencilBufferSize NOTIFY contextChanged);
    Q_PROPERTY(bool when READ when WRITE setWhen);

public:
    enum ContextType {
        TypeUnset   = -1,
        UnknownType = QSurfaceFormat::DefaultRenderableType,
        OpenGL      = QSurfaceFormat::OpenGL,
        OpenGLES    = QSurfaceFormat::OpenGLES
    };
    enum ContextProfile {
        ProfileUnset         = -1,
        NoProfile            = QSurfaceFormat::NoProfile,
        CoreProfile          = QSurfaceFormat::CoreProfile,
        CompatibilityProfile = QSurfaceFormat::CompatibilityProfile
    };

    HsQMLContextControl(QQuickItem* = NULL);
    ~HsQMLContextControl();

    Q_SIGNAL void contextChanged();
    int majorVersion();
    void setMajorVersion(int);
    void unsetMajorVersion();
    int minorVersion();
    void setMinorVersion(int);
    void unsetMinorVersion();
    ContextType contextType();
    void setContextType(ContextType);
    void unsetContextType();
    ContextProfile contextProfile();
    void setContextProfile(ContextProfile);
    void unsetContextProfile();
    bool deprecatedFunctions();
    void setDeprecatedFunctions(bool, bool = true);
    void unsetDeprecatedFunctions();
    int depthBufferSize();
    void setDepthBufferSize(int);
    void unsetDepthBufferSize();
    int stencilBufferSize();
    void setStencilBufferSize(int);
    void unsetStencilBufferSize();
    bool when();
    void setWhen(bool);

private:
    Q_DISABLE_COPY(HsQMLContextControl);

    Q_SLOT void doWindowChanged(QQuickWindow*);
    Q_SLOT void doSceneGraphInit();
    void classBegin();
    void componentComplete();
    void controlContext();
    QQuickWindow* mWindow;
    QSurfaceFormat mOriginal;
    QSurfaceFormat mCurrent;
    int mMajorVersion;
    int mMinorVersion;
    ContextType mContextType;
    ContextProfile mContextProfile;
    bool mDeprecatedFunctions;
    bool mDeprecatedFunctionsSet;
    int mDepthBufferSize;
    int mStencilBufferSize;
    bool mWhen;
    bool mDefer;
    bool mPending;
};

#endif //HSQML_CANVAS_H
