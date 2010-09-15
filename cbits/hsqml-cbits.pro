TEMPLATE = lib
CONFIG += qt lib
QT += declarative
target.path = /
INSTALLS += target
HEADERS += HsQMLManager.h HsQMLEngine.h HsQMLWindow.h HsQMLClass.h HsQMLObject.h hsqml.h
SOURCES += HsQMLManager.cpp HsQMLEngine.cpp HsQMLWindow.cpp HsQMLClass.cpp HsQMLObject.cpp HsQMLIntrinsics.cpp
QMAKE_CXXFLAGS += $(HSFFI_CFLAGS)
