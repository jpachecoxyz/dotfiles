import QtQuick
import Quickshell
import qs.Commons
import qs.Ui

// Pomodoro popup: status hero, transport controls, interval steppers, and
// general toggles. State and persistence live in the bar widget (BarWidget.qml);
// this panel reads through `host` and writes back via host.applySettings(),
// which mirrors the clock's local-apply-then-shell.json-write flow.

Panel {
  id: root

  moduleName: "javier.pomodoro"
  ipcTarget: "javier.pomodoro"
  manageIpc: false

  property var anchorItem: null
  property var hostWidget: null
  readonly property var host: hostWidget || null

  readonly property color contentForeground: bar ? bar.foreground : Color.foreground
  readonly property string contentFontFamily: bar ? bar.fontFamily : Style.font.family
  readonly property bool isRunning: host ? host.running : false
  readonly property color phaseColor: {
    if (!host) return contentForeground
    if (!host.running) return contentForeground
    return host.phase === "focus"
      ? (bar ? bar.urgent : Color.urgent) : Color.accent
  }

  function open() { root.controller.show() }

  KeyboardPanel {
    id: panel

    anchorItem: root.anchorItem
    owner: root.hostWidget || root
    bar: root.bar
    open: root.opened
    centerOnBar: true
    contentWidth: panel.fittedContentWidth(Style.space(300))
    contentHeight: panel.fittedContentHeight(contentColumn.implicitHeight)

    Flickable {
      anchors.fill: parent
      contentWidth: width
      contentHeight: contentColumn.implicitHeight
      clip: true
      boundsBehavior: Flickable.StopAtBounds
      interactive: contentHeight > height

      Column {
        id: contentColumn

        width: parent.width
        spacing: Style.space(10)

        // ---- Hero: phase + time.
        Item {
          width: parent.width
          height: heroColumn.implicitHeight

          Column {
            id: heroColumn

            anchors.centerIn: parent
            spacing: Style.space(2)

            Row {
              anchors.horizontalCenter: parent.horizontalCenter
              spacing: Style.space(8)

              Text {
                text: host ? host.phaseIcon : "\ue01b"
                renderType: Text.QtRendering
                font.family: "Material Symbols Rounded"
                font.variableAxes: ({ "FILL": root.isRunning ? 1 : 0 })
                font.pixelSize: Style.fontPx(1.6)
                color: root.phaseColor
                anchors.verticalCenter: parent.verticalCenter
              }

              Text {
                text: host ? host.timeText : "25:00"
                font.family: root.contentFontFamily
                font.pixelSize: Style.fontPx(2.2)
                font.weight: Font.DemiBold
                color: root.phaseColor
                anchors.verticalCenter: parent.verticalCenter
              }
            }

            Text {
              anchors.horizontalCenter: parent.horizontalCenter
              text: {
                if (!host) return ""
                var state = host.running ? qsTr("en curso") : qsTr("en pausa")
                return host.phaseLabel + " · " + state + " · "
                  + qsTr("%n bloque(s)", "", host.completedFocus)
              }
              font.family: root.contentFontFamily
              font.pixelSize: Style.font.caption
              color: Color.muted
            }
          }
        }

        // ---- Transport controls.
        Row {
          anchors.horizontalCenter: parent.horizontalCenter
          spacing: Style.space(8)

          Button {
            text: root.isRunning ? qsTr("⏸ Pausar") : qsTr("▶ Iniciar")
            onClicked: if (root.host) root.host.toggle()
          }

          Button {
            text: qsTr("⏭ Saltar")
            tooltipText: qsTr("Avanzar a la siguiente fase")
            onClicked: if (root.host) root.host.skipPhase()
          }

          Button {
            text: qsTr("↺ Reiniciar")
            tooltipText: qsTr("Volver al ciclo inicial")
            onClicked: if (root.host) root.host.reset()
          }
        }

        PanelSeparator {}

        // ---- Interval steppers.
        Text {
          anchors.left: parent.left
          anchors.leftMargin: Style.space(4)
          text: qsTr("Intervalos")
          font.family: root.contentFontFamily
          font.pixelSize: Style.font.caption
          color: Color.muted
        }

        StepperRow {
          label: qsTr("Enfoque")
          value: host ? host.workMin : 25
          minValue: 1
          maxValue: 90
          unit: qsTr("min")
          onStepped: function (v) {
            if (root.host) root.host.applySettings({ work: v })
          }
        }

        StepperRow {
          label: qsTr("Descanso corto")
          value: host ? host.shortMin : 5
          minValue: 1
          maxValue: 30
          unit: qsTr("min")
          onStepped: function (v) {
            if (root.host) root.host.applySettings({ shortBreak: v })
          }
        }

        StepperRow {
          label: qsTr("Descanso largo")
          value: host ? host.longMin : 15
          minValue: 1
          maxValue: 60
          unit: qsTr("min")
          onStepped: function (v) {
            if (root.host) root.host.applySettings({ longBreak: v })
          }
        }

        StepperRow {
          label: qsTr("Bloques hasta el largo")
          value: host ? host.interval : 4
          minValue: 2
          maxValue: 8
          unit: ""
          onStepped: function (v) {
            if (root.host) root.host.applySettings({ longBreakInterval: v })
          }
        }

        PanelSeparator {}

        // ---- General toggles.
        Toggle {
          width: parent.width
          label: qsTr("Auto-iniciar fases")
          description: qsTr("Encadena enfoque y descansos sin tocar nada")
          checked: host ? host.autoStart : true
          onClicked: if (root.host) root.host.applySettings({ autoStart: !host.autoStart })
        }

        Toggle {
          width: parent.width
          label: qsTr("Notificaciones")
          description: qsTr("Avisar cuando termina cada fase")
          checked: host ? host.notificationsEnabled : true
          onClicked: if (root.host) root.host.applySettings({ notify: !host.notificationsEnabled })
        }
      }
    }

    component StepperRow: Item {
      id: stepper

      property string label: ""
      property int value: 0
      property int minValue: 1
      property int maxValue: 99
      property string unit: ""

      signal stepped(int v)

      width: parent.width
      height: Math.max(row.implicitHeight, Style.space(34))

      function step(delta) {
        var next = Math.min(maxValue, Math.max(minValue, value + delta))
        if (next === value) return
        value = next
        stepper.stepped(next)
      }

      Row {
        id: row

        anchors.verticalCenter: parent.verticalCenter
        anchors.right: parent.right
        spacing: Style.space(6)

        Text {
          text: stepper.label
          font.family: root.contentFontFamily
          font.pixelSize: Style.font.body
          color: root.contentForeground
          anchors.verticalCenter: parent.verticalCenter
        }

        Item { width: Style.space(6); height: 1 }

        Button {
          width: Style.space(26)
          text: "−"
          enabled: stepper.value > stepper.minValue
          onClicked: stepper.step(-1)
        }

        Text {
          text: stepper.value + (stepper.unit !== "" ? " " + stepper.unit : "")
          font.family: root.contentFontFamily
          font.pixelSize: Style.font.body
          color: root.contentForeground
          anchors.verticalCenter: parent.verticalCenter
        }

        Button {
          width: Style.space(26)
          text: "+"
          enabled: stepper.value < stepper.maxValue
          onClicked: stepper.step(1)
        }
      }
    }
  }
}
