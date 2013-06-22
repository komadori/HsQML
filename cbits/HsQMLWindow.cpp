#include "hsqml.h"
#include "HsQMLManager.h"
#include "HsQMLWindow.h"

HsQMLWindow::HsQMLWindow(HsQMLEngine* engine)
    : QObject(engine)
    , mEngine(engine)
    , mContext(engine->engine())
    , mView(&mScene)
    , mComponent(NULL)
{
    // Setup context
    mContext.setContextProperty("window", this);

    // Handle Window close event manually
    mWindow.setAttribute(Qt::WA_DeleteOnClose, false);
    mWindow.installEventFilter(this);

    // Setup for QML performance
    mView.setOptimizationFlags(QGraphicsView::DontSavePainterState);
    mView.setViewportUpdateMode(QGraphicsView::BoundingRectViewportUpdate);
    mScene.setItemIndexMethod(QGraphicsScene::NoIndex);

    // Setup for QML key handling
    mView.viewport()->setFocusPolicy(Qt::NoFocus);
    mView.setFocusPolicy(Qt::StrongFocus);
    mScene.setStickyFocus(true);

    mWindow.setCentralWidget(&mView);
}

HsQMLWindow::~HsQMLWindow()
{
}

bool HsQMLWindow::eventFilter(QObject* obj, QEvent* ev)
{
    if (obj == &mWindow && ev->type() == QEvent::Close) {
        deleteLater();
    }
    return false;
}

QUrl HsQMLWindow::source() const
{
    return mSource;
}

void HsQMLWindow::setSource(const QUrl& url)
{
    mSource = url;
    if (mComponent) {
        delete mComponent;
        mComponent = NULL;
    }
    if (!mSource.isEmpty()) {
        mComponent = new QDeclarativeComponent(
            mEngine->engine(), mSource, this);
        if (mComponent->isLoading()) {
            QObject::connect(
                mComponent,
                SIGNAL(statusChanged(QDeclarativeComponent::Status)),
                this,
                SLOT(completeSetSource()));
        }
        else {
            completeSetSource();
        }
    }
}

void HsQMLWindow::completeSetSource()
{
    QObject::disconnect(
        mComponent, SIGNAL(statusChanged(QDeclarativeComponent::Status)),
        this, SLOT(completeSetSource()));
    QDeclarativeItem* item =
        qobject_cast<QDeclarativeItem*>(mComponent->create(&mContext));
    if (item) {
        mScene.addItem(item);
    }
}

QString HsQMLWindow::title() const
{
    return mWindow.windowTitle();
}

void HsQMLWindow::setTitle(const QString& title)
{
    mWindow.setWindowTitle(title);
}

bool HsQMLWindow::visible() const
{
    return mWindow.isVisible();
}

void HsQMLWindow::setVisible(bool visible)
{
    mWindow.setVisible(visible);
}

void HsQMLWindow::close()
{
    deleteLater();
}
