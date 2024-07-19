import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

Window {
  visible: true
  width: 300
  height: 200
  title: "Example"

  ColumnLayout {
    anchors.fill: parent
    spacing: 6

    RowLayout {
      Layout.fillWidth: true
      TextField {
        Layout.fillWidth: true
        id: name
        placeholderText: "Name"
      }

      TextField {
        id: age
        placeholderText: "Age"
        validator: IntValidator {bottom: 0}
        Layout.fillWidth: true
      }

      Button {
        text: "Welcome me"
        onClicked: hs.welcome(name.text, age.text)
      }
    }

    Label {
      text: hs.welcomeMsg
      Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
    }

    RowLayout {
      Layout.fillWidth: true
      id: row
      spacing: 6


      Button {
        id: button
        text: "-"
        Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
        onClicked: hs.decrement()
      }
      Label {
        Layout.fillWidth: true
        id: label
        horizontalAlignment: Text.AlignHCenter
        text: "Value is: " + parseInt(hs.counter)
      }

      Button {
        id: button1
        text: "+"
        Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
        onClicked: hs.increment()
      }
    }
    Repeater {
      model: hs.fun
      Label {
        text: modelData
      }
    }
  }
}

