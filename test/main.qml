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
      spacing: 2
      TextField {
        id: nameBox
        placeholderText: qsTr("Enter name")
      }

      Label {
        text: haskell.value
      }
      
      Button {
          text: "Hello World"
          onClicked: haskell.nameage(nameBox.text , 42)
      }
    }
}
