/*
This is a UI file (.ui.qml) that is intended to be edited in Qt Design Studio only.
It is supposed to be strictly declarative and only uses a subset of QML. If you edit
this file manually, you might introduce QML code that is not supported by Qt Design Studio.
Check out https://doc.qt.io/qtcreator/creator-quick-ui-forms.html for details on .ui.qml files.
*/

import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

Window {
    visible: true
    width: 200
    height: 55
    title: "Example"
    
    RowLayout {
        id: row
        anchors.fill: parent
        spacing: 6
        
        
        Button {
            id: button
            text: "-"
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
            onClicked: hs.decrement()
        }
        Label {
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
            id: label
            text: "Value is: " + parseInt(hs.counter)
        }
        
        Button {
            id: button1
            text: "+"
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
            onClicked: hs.increment()
        }
    }
}

