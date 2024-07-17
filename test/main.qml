import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import QtQuick.Window

ApplicationWindow {
    width: 400
    height: 300
    title: "SimpleData"

    Component.onCompleted: visible = true

    ColumnLayout {
        anchors.fill: parent
        SpinBox { value: qVar1}
        TextField { text: qVar2}
        CheckBox { checked: qVar3}
    }
}