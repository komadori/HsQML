#include "Canvas.h"
#include "Manager.h"

#include <QtCore/qmath.h>
#include <QtQuick/QSGSimpleTextureNode>
#include <QtQuick/QSGTexture>
#include <QtQuick/QSGTransformNode>
#include <QtQuick/QQuickWindow>

HsQMLGLCallbacks::HsQMLGLCallbacks(
    HsQMLGLSetupCb setupCb, HsQMLGLCleanupCb cleanupCb,
    HsQMLGLSyncCb syncCb, HsQMLGLPaintCb paintCb)
    : mSetupCb(setupCb)
    , mCleanupCb(cleanupCb)
    , mSyncCb(syncCb)
    , mPaintCb(paintCb)
{}

HsQMLGLCallbacks::~HsQMLGLCallbacks()
{
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mSyncCb));
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mPaintCb));
}

HsQMLGLDelegateImpl::HsQMLGLDelegateImpl(HsQMLGLMakeCallbacksCb makeContextCb)
    : mMakeCallbacksCb(makeContextCb)
{}

HsQMLGLDelegateImpl::~HsQMLGLDelegateImpl()
{
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mMakeCallbacksCb));
}

HsQMLGLDelegate::HsQMLGLDelegate()
{
}

HsQMLGLDelegate::~HsQMLGLDelegate()
{
}

void HsQMLGLDelegate::setup(HsQMLGLMakeCallbacksCb makeContextCb)
{
    mImpl = new HsQMLGLDelegateImpl(makeContextCb);
}

bool HsQMLGLDelegate::isValid()
{
    return mImpl;
}

HsQMLGLDelegate::CallbacksRef HsQMLGLDelegate::makeCallbacks()
{
    CallbacksRef dataPtr;
    if (mImpl) {
        HsQMLGLSetupCb setupCb;
        HsQMLGLCleanupCb cleanupCb;
        HsQMLGLSyncCb syncCb;
        HsQMLGLPaintCb paintCb;
        mImpl->mMakeCallbacksCb(&setupCb, &cleanupCb, &syncCb, &paintCb);
        dataPtr = new HsQMLGLCallbacks(setupCb, cleanupCb, syncCb, paintCb);
    }
    return dataPtr;
}

HsQMLWindowInfoImpl::HsQMLWindowInfoImpl(QQuickWindow* win)
    : mWin(win)
    , mBelowCount(0)
    , mBelowClear(false)
{}

HsQMLWindowInfo::HsQMLWindowInfo()
{}

HsQMLWindowInfo HsQMLWindowInfo::getWindowInfo(QQuickWindow* win)
{
    const char* propName = "_hsqml_window_info";
    HsQMLWindowInfo info = win->property(propName).value<HsQMLWindowInfo>();
    if (!info.mImpl) {
        info.mImpl = new HsQMLWindowInfoImpl(win);
        win->setProperty(propName, QVariant::fromValue(info));
    }
    return info;
}

void HsQMLWindowInfo::addBelow()
{
    Q_ASSERT(mImpl);
    if (0 == mImpl->mBelowCount++) {
        mImpl->mWin->setClearBeforeRendering(false);
    }
}

void HsQMLWindowInfo::removeBelow()
{
    Q_ASSERT(mImpl);
    if (0 == --mImpl->mBelowCount) {
        mImpl->mWin->setClearBeforeRendering(true);
    }
}

bool HsQMLWindowInfo::needsBelowClear()
{
    Q_ASSERT(mImpl);
    if (mImpl->mBelowCount && !mImpl->mBelowClear) {
        mImpl->mBelowClear = true;
        return true;
    }
    return false;
}

void HsQMLWindowInfo::endFrame()
{
    Q_ASSERT(mImpl);
    mImpl->mBelowClear = false;
}

HsQMLCanvasBackEnd::HsQMLCanvasBackEnd(
    QQuickWindow* win,
    const HsQMLGLDelegate::CallbacksRef& cbs,
    HsQMLCanvas::DisplayMode mode)
    : mWindow(win)
    , mWinInfo(HsQMLWindowInfo::getWindowInfo(win))
    , mGLCallbacks(cbs)
    , mGL(NULL)
    , mStatus(HsQMLCanvas::Okay)
    , mDisplayMode(mode)
    , mItemWidth(0)
    , mItemHeight(0)
    , mTransformNode(NULL)
    , mCanvasWidth(0)
    , mCanvasHeight(0)
{
    if (HsQMLCanvas::Above == mDisplayMode) {
        QObject::connect(
            win, SIGNAL(afterRendering()),
            this, SLOT(doRendering()));
    }
    else {
        QObject::connect(
            win, SIGNAL(beforeRendering()),
            this, SLOT(doRendering()));

        if (HsQMLCanvas::Below == mDisplayMode) {
            mWinInfo.addBelow();
            QObject::connect(
                win, SIGNAL(frameSwapped()),
                this, SLOT(doEndFrame()));
        }
    }
}

HsQMLCanvasBackEnd::~HsQMLCanvasBackEnd()
{
    if (HsQMLCanvas::Below == mDisplayMode) {
        mWinInfo.removeBelow();
    }
}

void HsQMLCanvasBackEnd::setTransformNode(
    QSGTransformNode* tn, qreal w, qreal h)
{
    mTransformNode = tn;
    mItemWidth = w;
    mItemHeight = h;
}

QSGTexture* HsQMLCanvasBackEnd::updateFBO(qreal w, qreal h)
{
    if (HsQMLCanvas::Inline == mDisplayMode) {
        if (!mFBO || w != mCanvasWidth || h != mCanvasHeight) {
            mCanvasWidth = w;
            mCanvasHeight = h;
            QSize dims(qCeil(mCanvasWidth), qCeil(mCanvasHeight));
            mFBO.reset(new QOpenGLFramebufferObject(
                dims, QOpenGLFramebufferObject::Depth));
            mTexture.reset(mWindow->createTextureFromId(
                mFBO->texture(), dims, QQuickWindow::TextureHasAlphaChannel));
        }
    }
    else {
        mTexture.reset();
        mFBO.reset();
    }
    return mTexture.data();
}

HsQMLCanvas::Status HsQMLCanvasBackEnd::status() const
{
    return mStatus;
}

void HsQMLCanvasBackEnd::setStatus(HsQMLCanvas::Status status)
{
    bool change = mStatus != status;
    mStatus = status;
    if (change) {
        statusChanged(mStatus);
    }
}

void HsQMLCanvasBackEnd::doRendering()
{
    if (!mGL) {
        mGL = mWindow->openglContext();
        QObject::connect(
            mGL, SIGNAL(aboutToBeDestroyed()), this, SLOT(doCleanup()));
        HsQMLGLCanvasType ctype;
        QSurfaceFormat format = mGL->format();
        switch (format.renderableType()) {
            case QSurfaceFormat::OpenGL: ctype = HSQML_GL_DESKTOP; break;
            case QSurfaceFormat::OpenGLES: ctype = HSQML_GL_ES; break;
            default: setStatus(HsQMLCanvas::BadConfig); return;
        }
        mGLViewportFn = reinterpret_cast<GLViewportFn>(
            mGL->getProcAddress("glViewport"));
        mGLClearColorFn = reinterpret_cast<GLClearColorFn>(
            mGL->getProcAddress("glClearColor"));
        mGLClearFn = reinterpret_cast<GLClearFn>(
            mGL->getProcAddress("glClear"));
        if (!mGLViewportFn || !mGLClearColorFn || !mGLClearFn) {
            setStatus(HsQMLCanvas::BadProcs);
            return;
        }
        mGLCallbacks->mSetupCb(
            ctype, format.majorVersion(), format.minorVersion());
    }

    // Reset OpenGL state before rendering
#if QT_VERSION >= 0x050200
    mWindow->resetOpenGLState();
#else
#warning Resetting OpenGL state requires Qt 5.2 or later
#endif

    // Clear window if painting below the scenegraph
    if (mWinInfo.needsBelowClear()) {
        QColor bg = mWindow->color();
        mGLClearColorFn(bg.redF(), bg.greenF(), bg.blueF(), bg.alphaF());
        mGLClearFn(GL_COLOR_BUFFER_BIT);
    }

    // Setup prior to paint callback
    QMatrix4x4 matrix;
    bool inlineMode = HsQMLCanvas::Inline == mDisplayMode;
    if (inlineMode) {
        if (!mFBO->bind()) {
            setStatus(HsQMLCanvas::BadBind);
            return;
        }
        mGLViewportFn(0, 0, qCeil(mCanvasWidth), qCeil(mCanvasHeight));

        // Clear FBO to transparent
        mGLClearColorFn(0, 0, 0, 0);
        mGLClearFn(GL_COLOR_BUFFER_BIT);
    }
    else {
        // Calculate matrix for non-inline display modes
        QMatrix4x4 smatrix;
        QSGNode* node = mTransformNode;
        while (node) {
            if (QSGNode::TransformNodeType == node->type()) {
                QSGTransformNode* tnode = static_cast<QSGTransformNode*>(node);
                smatrix = tnode->matrix() * smatrix;
            }
            node = node->parent();
        }
        matrix.translate(-1, 1);
        matrix.scale(2.0f/mWindow->width(), -2.0f/mWindow->height());
        matrix *= smatrix;
        matrix.scale(mItemWidth/2.0f, mItemHeight/2.0f);
        matrix.translate(1, 1);

        mGLViewportFn(0, 0, mWindow->width(), mWindow->height());
    }

    setStatus(HsQMLCanvas::Okay);
    mGLCallbacks->mPaintCb(matrix.data(), mItemWidth, mItemHeight);

    if (inlineMode) {
        mFBO->release();
    }
}

void HsQMLCanvasBackEnd::doEndFrame()
{
    mWinInfo.endFrame();
}

void HsQMLCanvasBackEnd::doCleanup()
{
    if (mGL) {
        mGL->makeCurrent(mWindow);
        mGL = NULL;

        mTexture.reset();
        mFBO.reset();

        mGLCallbacks->mCleanupCb();
    }
}

void HsQMLCanvasBackEnd::doCleanupKill()
{
    doCleanup();
    delete this;
}

HsQMLCanvas::HsQMLCanvas(QQuickItem* parent)
    : QQuickItem(parent)
    , mWindow(NULL)
    , mBackEnd(NULL)
    , mStatus(Okay)
    , mFrontStatus(Okay)
    , mBackStatus(Okay)
    , mDisplayMode(Inline)
    , mCanvasWidth(0)
    , mCanvasWidthSet(false)
    , mCanvasHeight(0)
    , mCanvasHeightSet(false)
    , mLoadModel(false)
    , mValidModel(false)
{
    QObject::connect(
        this, SIGNAL(windowChanged(QQuickWindow*)),
        this, SLOT(doWindowChanged(QQuickWindow*)));
    setFlag(ItemHasContents, true);
}

HsQMLCanvas::~HsQMLCanvas()
{
    detachBackEnd();
}

void HsQMLCanvas::geometryChanged(const QRectF& rect, const QRectF&)
{
    if (!mCanvasWidthSet) {
        setCanvasWidth(rect.width(), false);
    }
    if (!mCanvasHeightSet) {
        setCanvasHeight(rect.height(), false);
    }
    update();
}

QSGNode* HsQMLCanvas::updatePaintNode(
    QSGNode* oldNode, UpdatePaintNodeData* paintData)
{
    // Window always needs repainting
    mWindow->update();

    // Lazily create new callbacks
    if (!mGLCallbacks) {
        mGLCallbacks = mDelegate.value<HsQMLGLDelegate>().makeCallbacks();
        mLoadModel = true;
        mValidModel = false;

        // Display nothing without a valid delegate
        if (!mGLCallbacks) {
            setStatus(BadDelegate);
            delete oldNode;
            return NULL;
        }
    }

    // Process model update
    if (mLoadModel) {
        mValidModel = mGLCallbacks->mSyncCb(
            reinterpret_cast<HsQMLJValHandle*>(&mModel));
        mLoadModel = false;
    }

    // Display nothing if there's no valid model
    if (!mValidModel) {
        setStatus(BadModel);
        detachBackEnd();
        delete oldNode;
        return NULL;
    }

    // Create back-end on the rendering thread
    if (!mBackEnd) {
        mBackEnd = new HsQMLCanvasBackEnd(mWindow, mGLCallbacks, mDisplayMode);

        // Monitor back-end's status
        QObject::connect(
            mBackEnd, SIGNAL(statusChanged(HsQMLCanvas::Status)),
            this, SLOT(doBackEndStatusChanged(HsQMLCanvas::Status)));
        setStatus(mBackEnd->status(), true);
    }
    setStatus(Okay);

    // Save pointer to transform node
    mBackEnd->setTransformNode(
        Inline != mDisplayMode ? paintData->transformNode : NULL,
        width(), height());

    // Produce texture node if needed
    if (QSGTexture* texture =
            mBackEnd->updateFBO(mCanvasWidth, mCanvasHeight)) {
        QSGSimpleTextureNode* n = static_cast<QSGSimpleTextureNode*>(oldNode);
        if (!n) {
            n = new QSGSimpleTextureNode();
        }
        n->setTexture(texture);
        n->setRect(0, 0, width(), height());
        return n;
    }

    delete oldNode;
    return NULL;
}

void HsQMLCanvas::detachBackEnd()
{
    if (mBackEnd) {
        // The back-end belongs to the rendering thread
        QMetaObject::invokeMethod(
            mBackEnd, "doCleanupKill", Qt::QueuedConnection);
        QObject::disconnect(mBackEnd, 0, this, 0);
        mBackEnd = NULL;
        mGLCallbacks.reset();
    }
}

HsQMLCanvas::DisplayMode HsQMLCanvas::displayMode() const
{
    return mDisplayMode;
}

void HsQMLCanvas::setDisplayMode(HsQMLCanvas::DisplayMode mode)
{
    bool change = mDisplayMode != mode;
    mDisplayMode = mode;
    if (change) {
        detachBackEnd();
        displayModeChanged();
        update();
    }
}

qreal HsQMLCanvas::canvasWidth() const
{
    return mCanvasWidth;
}

void HsQMLCanvas::setCanvasWidth(qreal w, bool set)
{
    bool change = mCanvasWidth != w;
    mCanvasWidth = w;
    mCanvasWidthSet |= set;
    if (change) {
        canvasWidthChanged();
        update();
    }
}

void HsQMLCanvas::unsetCanvasWidth()
{
    mCanvasWidthSet = false;
    setCanvasWidth(width(), false);
}

qreal HsQMLCanvas::canvasHeight() const
{
    return mCanvasHeight;
}

void HsQMLCanvas::setCanvasHeight(qreal h, bool set)
{
    bool change = mCanvasHeight != h;
    mCanvasHeight = h;
    mCanvasHeightSet |= set;
    if (change) {
        canvasHeightChanged();
        update();
    }
}

void HsQMLCanvas::unsetCanvasHeight()
{
    mCanvasHeightSet = false;
    setCanvasHeight(height(), false);
}

QVariant HsQMLCanvas::delegate() const
{
    return mDelegate;
}

void HsQMLCanvas::setDelegate(const QVariant& d)
{
    bool change = mDelegate != d;
    mDelegate = d;
    if (change) {
        detachBackEnd();
        delegateChanged();
        update();
    }
}

QJSValue HsQMLCanvas::model() const
{
    return mModel;
}

void HsQMLCanvas::setModel(const QJSValue& m)
{
    bool change = !mModel.strictlyEquals(m);
    mModel = m;
    if (change) {
        modelChanged();
        mLoadModel = true;
        update();
    }
}

HsQMLCanvas::Status HsQMLCanvas::status() const
{
    return mStatus;
}

void HsQMLCanvas::setStatus(Status status, bool backEnd)
{
    if (backEnd) {
        mBackStatus = status;
    }
    else {
        mFrontStatus = status;
    }
    Status newStatus = mFrontStatus == Okay ? mBackStatus : mFrontStatus;
    bool change = mStatus != newStatus;
    if (change) {
        statusChanged();
    }
}

void HsQMLCanvas::doWindowChanged(QQuickWindow* win)
{
    detachBackEnd();
    mWindow = win;
}

void HsQMLCanvas::doBackEndStatusChanged(Status status)
{
    setStatus(status, true);
}

HsQMLContextControl::HsQMLContextControl(QQuickItem* parent)
    : QQuickItem(parent)
    , mWindow(NULL)
    , mMajorVersion(-1)
    , mMinorVersion(-1)
    , mContextType(TypeUnset)
    , mContextProfile(ProfileUnset)
    , mDeprecatedFunctions(false)
    , mDeprecatedFunctionsSet(false)
    , mDepthBufferSize(0)
    , mStencilBufferSize(0)
    , mWhen(true)
    , mDefer(false)
    , mPending(false)
{
    QObject::connect(
        this, SIGNAL(windowChanged(QQuickWindow*)),
        this, SLOT(doWindowChanged(QQuickWindow*)));
}

HsQMLContextControl::~HsQMLContextControl()
{
}

int HsQMLContextControl::majorVersion()
{
    return mCurrent.majorVersion();
}

void HsQMLContextControl::setMajorVersion(int major)
{
    bool change = mMajorVersion != major;
    mMajorVersion = major;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetMajorVersion()
{
    setMajorVersion(-1);
}

int HsQMLContextControl::minorVersion()
{
    return mCurrent.minorVersion();
}

void HsQMLContextControl::setMinorVersion(int minor)
{
    bool change = mMinorVersion != minor;
    mMinorVersion = minor;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetMinorVersion()
{
    setMinorVersion(-1);
}

HsQMLContextControl::ContextType HsQMLContextControl::contextType()
{
    return static_cast<ContextType>(mCurrent.renderableType());
}

void HsQMLContextControl::setContextType(ContextType type)
{
    bool change = mContextType != type;
    mContextType = type;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetContextType()
{
    setContextType(TypeUnset);
}

HsQMLContextControl::ContextProfile HsQMLContextControl::contextProfile()
{
    return static_cast<ContextProfile>(mCurrent.profile());
}

void HsQMLContextControl::setContextProfile(ContextProfile profile)
{
    bool change = mContextProfile != profile;
    mContextProfile = profile;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetContextProfile()
{
    setContextProfile(ProfileUnset);
}

bool HsQMLContextControl::deprecatedFunctions()
{
    return mCurrent.testOption(QSurfaceFormat::DeprecatedFunctions);
}

void HsQMLContextControl::setDeprecatedFunctions(bool df, bool set)
{
    bool change = (mDeprecatedFunctionsSet != set) ||
        (mDeprecatedFunctionsSet && (mDeprecatedFunctions != df));
    mDeprecatedFunctions = df;
    mDeprecatedFunctionsSet = set;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetDeprecatedFunctions()
{
    setDeprecatedFunctions(false, false);
}

int HsQMLContextControl::depthBufferSize()
{
    return mCurrent.depthBufferSize();
}

void HsQMLContextControl::setDepthBufferSize(int size)
{
    bool change = mDepthBufferSize != size;
    mDepthBufferSize = size;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetDepthBufferSize()
{
    setDepthBufferSize(0);
}

int HsQMLContextControl::stencilBufferSize()
{
    return mCurrent.stencilBufferSize();
}

void HsQMLContextControl::setStencilBufferSize(int size)
{
    bool change = mStencilBufferSize != size;
    mStencilBufferSize = size;
    if (change) {
        controlContext();
    }
}

void HsQMLContextControl::unsetStencilBufferSize()
{
    setStencilBufferSize(0);
}

bool HsQMLContextControl::when()
{
    return mWhen;
}

void HsQMLContextControl::setWhen(bool when)
{
    mWhen = when;
    if (mPending && !mDefer && mWhen) {
        controlContext();
    }
}

void HsQMLContextControl::classBegin()
{
    mDefer = true;
}

void HsQMLContextControl::componentComplete()
{
    mDefer = false;
    if (mPending && mWhen) {
        controlContext();
    }
}

void HsQMLContextControl::doWindowChanged(QQuickWindow* win)
{
    if (mWindow) {
        QObject::disconnect(mWindow, 0, this, 0);
    }
    mWindow = win;
    if (mWindow) {
        QObject::connect(
            mWindow, SIGNAL(sceneGraphInitialized()),
            this, SLOT(doSceneGraphInit()));
        mOriginal = mWindow->requestedFormat();
        mCurrent = mWindow->openglContext() ?
            mWindow->openglContext()->format() : mWindow->format();
    }
    else {
        mOriginal = QSurfaceFormat();
        mCurrent = QSurfaceFormat();
    }
    contextChanged();

    if ((mMajorVersion >=0) || (mMinorVersion >= 0) ||
        (mContextType != TypeUnset) || (mContextProfile != ProfileUnset) ||
        !mDeprecatedFunctionsSet) {
        controlContext();
    }
}

void HsQMLContextControl::doSceneGraphInit()
{
    mCurrent = mWindow->openglContext()->format();
    contextChanged();
}

void HsQMLContextControl::controlContext()
{
    // Do nothing if no window
    if (!mWindow) {
        return;
    }

    // Do nothing if changes are being deferred
    if (mDefer || !mWhen) {
        mPending = true;
        return;
    }
    mPending = false;

    QSurfaceFormat fmt = mOriginal;
    if (mMajorVersion >= 0) {
        fmt.setMajorVersion(mMajorVersion);
    }
    if (mMinorVersion >= 0) {
        fmt.setMinorVersion(mMinorVersion);
    }
    if (mContextType >= 0) {
        fmt.setRenderableType(static_cast<QSurfaceFormat::RenderableType>(
            mContextType));
    }
    if (mContextProfile >= 0) {
        fmt.setProfile(static_cast<QSurfaceFormat::OpenGLContextProfile>(
            mContextProfile));
    }
    if (mDeprecatedFunctionsSet) {
#if QT_VERSION >= 0x050300
        fmt.setOption(QSurfaceFormat::DeprecatedFunctions,
            mDeprecatedFunctions);
#else
        if (mDeprecatedFunctions) {
            fmt.setOption(QSurfaceFormat::DeprecatedFunctions);
        }
#endif
    }
    fmt.setDepthBufferSize(qMax(fmt.depthBufferSize(), mDepthBufferSize));
    fmt.setStencilBufferSize(qMax(fmt.stencilBufferSize(), mStencilBufferSize));
    if (fmt == mWindow->requestedFormat()) {
        return;
    }
    mWindow->setFormat(fmt);

    // Recreate OpenGL context
    mWindow->setPersistentOpenGLContext(false);
    mWindow->setPersistentSceneGraph(false);
    bool visible = mWindow->isVisible();
    mWindow->destroy();
    mWindow->releaseResources();
    mWindow->setVisible(visible);
    mWindow->setPersistentOpenGLContext(true);
    mWindow->setPersistentSceneGraph(true);
}

HsQMLGLDelegateHandle* hsqml_create_gldelegate()
{
    return reinterpret_cast<HsQMLGLDelegateHandle*>(new HsQMLGLDelegate());
}

void hsqml_finalise_gldelegate_handle(
    HsQMLGLDelegateHandle* hndl)
{
    HsQMLGLDelegate* delegate = reinterpret_cast<HsQMLGLDelegate*>(hndl);
    delete delegate;
}

void hsqml_gldelegate_setup(
    HsQMLGLDelegateHandle* hndl,
    HsQMLGLMakeCallbacksCb makeContextCb)
{
    HsQMLGLDelegate* delegate = reinterpret_cast<HsQMLGLDelegate*>(hndl);
    delegate->setup(makeContextCb);
}

void hsqml_gldelegate_to_jval(
    HsQMLGLDelegateHandle* hndl,
    HsQMLJValHandle* jhndl)
{
    HsQMLGLDelegate* delegate = reinterpret_cast<HsQMLGLDelegate*>(hndl);
    new((void*)jhndl) QJSValue(
        gManager->activeEngine()->declEngine()->toScriptValue(*delegate));
}

int hsqml_gldelegate_from_jval(
    HsQMLGLDelegateHandle* hndl,
    HsQMLJValHandle* jhndl)
{
    HsQMLGLDelegate* delegate = reinterpret_cast<HsQMLGLDelegate*>(hndl);
    QJSValue* value = reinterpret_cast<QJSValue*>(jhndl);
    *delegate = gManager->activeEngine()->declEngine()->
        fromScriptValue<HsQMLGLDelegate>(*value); 
    return delegate->isValid();
}
