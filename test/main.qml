import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import QtQuick.Window

ApplicationWindow {
    width: 400
    height: 300
    title: "SimpleData"

    Component.onCompleted: visible = true

        Button {
            text: "Hello World"
            onClicked: haskell.hello()
        }
}