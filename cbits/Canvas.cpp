#include "Canvas.h"
#include "Manager.h"

#include <QtCore/qmath.h>
#include <QtQuick/QSGSimpleTextureNode>
#include <QtQuick/QSGTexture>
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

HsQMLCanvasBackEnd::HsQMLCanvasBackEnd(
    QQuickWindow* win, const HsQMLGLDelegate::CallbacksRef& cbs)
    : mWindow(win)
    , mGLCallbacks(cbs)
    , mGL(NULL)
    , mStatus(HsQMLCanvas::Okay)
{
    QObject::connect(
        win, SIGNAL(beforeRendering()),
        this, SLOT(doRendering()));
}

HsQMLCanvasBackEnd::~HsQMLCanvasBackEnd()
{
}

void HsQMLCanvasBackEnd::setModeSize(
    HsQMLCanvas::DisplayMode mode, qreal w, qreal h)
{
    mDisplayMode = mode;
    mCanvasWidth = w;
    mCanvasHeight = h;
}

QSGTexture* HsQMLCanvasBackEnd::updateFBO()
{
    if (HsQMLCanvas::Inline == mDisplayMode) {
        if (!mFBO || mFBO->width() != mCanvasWidth ||
                     mFBO->height() != mCanvasHeight) {
            mFBO.reset(new QOpenGLFramebufferObject(
                qCeil(mCanvasWidth), qCeil(mCanvasHeight),
                QOpenGLFramebufferObject::Depth));
            mTexture.reset(mWindow->createTextureFromId(
                mFBO->texture(), mFBO->size()));
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
        switch (mGL->format().renderableType()) {
            case QSurfaceFormat::OpenGL: ctype = HSQML_GL_DESKTOP; break;
            case QSurfaceFormat::OpenGLES: ctype = HSQML_GL_ES; break;
            default: setStatus(HsQMLCanvas::BadConfig); return;
        }
        mGLCallbacks->mSetupCb(ctype);
    }

    bool inlineMode = HsQMLCanvas::Inline == mDisplayMode;
    if (inlineMode) {
        if (!mFBO->bind()) {
            setStatus(HsQMLCanvas::BadBind);
            return;
        }
    }

    setStatus(HsQMLCanvas::Okay);
    mGLCallbacks->mPaintCb(mCanvasWidth, mCanvasHeight);

    if (inlineMode) {
        mFBO->release();
    }
}

void HsQMLCanvasBackEnd::doCleanup()
{
    mGL->makeCurrent(mWindow);
    mGL = NULL;

    mTexture.reset();
    mFBO.reset();

    mGLCallbacks->mCleanupCb();
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
}

QSGNode* HsQMLCanvas::updatePaintNode(
    QSGNode* oldNode, UpdatePaintNodeData* paintData)
{
    // Display nothing if there's no delegate
    if (!mGLCallbacks) {
        setStatus(BadDelegate);
        delete oldNode;
        return NULL;
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
        mBackEnd = new HsQMLCanvasBackEnd(mWindow, mGLCallbacks);

        // Monitor back-end's status
        QObject::connect(
            mBackEnd, SIGNAL(statusChanged(HsQMLCanvas::Status)),
            this, SLOT(doBackEndStatusChanged(HsQMLCanvas::Status)));
        setStatus(mBackEnd->status(), true);
    }
    mBackEnd->setModeSize(mDisplayMode, mCanvasWidth, mCanvasHeight);
    setStatus(Okay);

    // Produce texture node if needed
    if (QSGTexture* texture = mBackEnd->updateFBO()) {
        QSGSimpleTextureNode* n = static_cast<QSGSimpleTextureNode*>(oldNode);
        if (!n) {
            n = new QSGSimpleTextureNode();
        }
        n->setRect(0, 0, width(), height());
        n->setTexture(texture);
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
        mGLCallbacks = d.value<HsQMLGLDelegate>().makeCallbacks();
        detachBackEnd();
        delegateChanged();
        mLoadModel = true;
        mValidModel = false;
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

void hsqml_gldelegate_from_jval(
    HsQMLGLDelegateHandle* hndl,
    HsQMLJValHandle* jhndl)
{
    HsQMLGLDelegate* delegate = reinterpret_cast<HsQMLGLDelegate*>(hndl);
    QJSValue* value = reinterpret_cast<QJSValue*>(jhndl);
    *delegate = gManager->activeEngine()->declEngine()->
        fromScriptValue<HsQMLGLDelegate>(*value); 
}
