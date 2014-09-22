#include "Canvas.h"
#include "Manager.h"

#include <QtCore/qmath.h>
#include <QtQuick/QSGSimpleTextureNode>
#include <QtQuick/QSGTexture>
#include <QtQuick/QQuickWindow>

HsQMLGLContextData::HsQMLGLContextData(
    HsQMLGLSyncCb syncCb, HsQMLGLPaintCb paintCb)
    : mSyncCb(syncCb)
    , mPaintCb(paintCb)
{}

HsQMLGLContextData::~HsQMLGLContextData()
{
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mSyncCb));
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mPaintCb));
}

HsQMLGLDelegateImpl::HsQMLGLDelegateImpl(HsQMLGLMakeContextCb makeContextCb)
    : mMakeContextCb(makeContextCb)
{}

HsQMLGLDelegateImpl::~HsQMLGLDelegateImpl()
{
    gManager->freeFun(reinterpret_cast<HsFunPtr>(mMakeContextCb));
}

HsQMLGLDelegate::HsQMLGLDelegate()
{
}

HsQMLGLDelegate::~HsQMLGLDelegate()
{
}

void HsQMLGLDelegate::setup(HsQMLGLMakeContextCb makeContextCb)
{
    mImpl = new HsQMLGLDelegateImpl(makeContextCb);
}

HsQMLGLDelegate::ContextDataRef HsQMLGLDelegate::makeContextData()
{
    ContextDataRef dataPtr;
    if (mImpl) {
        HsQMLGLSyncCb syncCb;
        HsQMLGLPaintCb paintCb;
        mImpl->mMakeContextCb(&syncCb, &paintCb);
        dataPtr = new HsQMLGLContextData(syncCb, paintCb);
    }
    return dataPtr;
}

HsQMLCanvasBackEnd::HsQMLCanvasBackEnd(QQuickWindow* win)
    : mWindow(win)
{
    QObject::connect(
        win, SIGNAL(beforeRendering()),
        this, SLOT(doRendering()));
}

HsQMLCanvasBackEnd::~HsQMLCanvasBackEnd()
{
}

void HsQMLCanvasBackEnd::setGLContextData(
    const HsQMLGLDelegate::ContextDataRef& data)
{
    mGLContextData = data;
}

void HsQMLCanvasBackEnd::setModeSize(
    HsQMLCanvas::DisplayMode mode, qreal w, qreal h)
{
    mDisplayMode = mode;
    mCanvasWidth = w;
    mCanvasHeight = h;

    if (HsQMLCanvas::Inline == mode) {
        if (!mFBO || mFBO->width() != w || mFBO->height() != h) {
            mFBO.reset(new QOpenGLFramebufferObject(
                qCeil(w), qCeil(h), QOpenGLFramebufferObject::Depth));
            mTexture.reset(mWindow->createTextureFromId(
                mFBO->texture(), mFBO->size()));
        }
    }
    else {
        mFBO.reset();
        mTexture.reset();
    }
}

QSGTexture* HsQMLCanvasBackEnd::texture() const
{
    return mTexture.data();
}

void HsQMLCanvasBackEnd::doRendering()
{
    bool inlineMode = HsQMLCanvas::Inline == mDisplayMode;
    if (inlineMode && !mFBO) {
        return;
    }
    if (inlineMode) {
        mFBO->bind();
    }
    if (mGLContextData) {
        mGLContextData->mPaintCb(mCanvasWidth, mCanvasHeight);
    }
    if (inlineMode) {
        mFBO->release();
    }
}

HsQMLCanvas::HsQMLCanvas(QQuickItem* parent)
    : QQuickItem(parent)
    , mWindow(NULL)
    , mBackEnd(NULL)
    , mDisplayMode(Inline)
    , mCanvasWidth(0)
    , mCanvasWidthSet(false)
    , mCanvasHeight(0)
    , mCanvasHeightSet(false)
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
    // This executes on the rendering thread
    if (!mBackEnd) {
        mBackEnd = new HsQMLCanvasBackEnd(mWindow);
    }
    mBackEnd->setGLContextData(mGLContextData);
    mBackEnd->setModeSize(mDisplayMode, mCanvasWidth, mCanvasHeight);

    // Produce texture node if needed
    if (QSGTexture* texture = mBackEnd->texture()) {
        QSGSimpleTextureNode* n = static_cast<QSGSimpleTextureNode*>(oldNode);
        if (!n) {
            n = new QSGSimpleTextureNode();
        }
        n->setRect(0, 0, width(), height());
        n->setTexture(texture);
        return n;
    }
    return NULL;
}

void HsQMLCanvas::detachBackEnd()
{
    if (mBackEnd) {
        // The back-end belongs to the rendering thread
        mBackEnd->deleteLater();
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
    mDelegate = d;
    mGLContextData = d.value<HsQMLGLDelegate>().makeContextData();
    delegateChanged();
}

void HsQMLCanvas::doWindowChanged(QQuickWindow* win)
{
    detachBackEnd();
    mWindow = win;
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
    HsQMLGLMakeContextCb makeContextCb)
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
