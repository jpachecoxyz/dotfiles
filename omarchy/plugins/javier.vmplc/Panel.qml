import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui

// Dropdown for the PLC VM: bridge status, VM state, and one primary action
// that launches the VM or jumps to workspace 10 when it's already running.
// State lives in the bar widget (BarWidget.qml) and is polled there.

Panel {
  id: root

  moduleName: "javier.vmplc"
  ipcTarget: "javier.vmplc"
  manageIpc: false

  property var anchorItem: null
  property var hostWidget: null
  readonly property var host: hostWidget || null

  readonly property color contentForeground: bar ? bar.foreground : Color.foreground
  readonly property string contentFontFamily: bar ? bar.fontFamily : Style.font.family

  function statusColor() {
    if (!root.host) return Color.muted
    if (!root.host.configured) return Color.muted
    return bar ? bar.urgent : Color.urgent
  }

  function vmColor() {
    if (!root.host) return Color.muted
    if (!root.host.vmRunning) return Color.muted
    return bar ? bar.urgent : Color.urgent
  }

  function open() {
    root.controller.show()
    root.confirmDelete = false
    root.actionStatus = ""
  }

  property bool confirmDelete: false
  property string actionStatus: ""
  property bool actionUrgent: false

  function deleteBridge() {
    if (root.confirmDelete) {
      root.confirmDelete = false
      root.actionUrgent = false
      root.actionStatus = qsTr("Eliminando puente y TAP… (se pedirá la contraseña)")
      delProc.exec({
        command: [
          "pkexec",
          "env",
          "USER=" + Quickshell.env("USER"),
          Quickshell.env("HOME") + "/.local/bin/scripts/vm-set-ip",
          "--delete"
        ]
      })
    } else {
      root.confirmDelete = true
      root.actionUrgent = true
      root.actionStatus = qsTr("Se eliminará el puente, el TAP y la IP guardada. Pulsa de nuevo.")
    }
  }

  Process {
    id: delProc

    stdout: StdioCollector { id: delOut }
    stderr: StdioCollector { id: delErr }

    onExited: function (exitCode) {
      if (exitCode === 0) {
        root.actionUrgent = false
        root.actionStatus = qsTr("Red eliminada.")
        root.close()
        if (root.host) root.host.refreshStatus()
      } else {
        var lines = (delOut.text.trim() + "\n" + delErr.text.trim()).split(/\s*\n\s*/)
        root.actionStatus = lines[lines.length - 1] || ("Error (código " + exitCode + ")")
        root.actionUrgent = true
        root.confirmDelete = false
      }
    }
  }

  function activateVm() {
    if (root.host && root.host.vmRunning) {
      Quickshell.execDetached(["hyprctl", "dispatch", "workspace", "10"])
    } else {
      Quickshell.execDetached([
        Quickshell.env("HOME") + "/.local/bin/scripts/vm-plc"
      ])
    }
    root.close()
  }

  function openAssistant() {
    Quickshell.execDetached(["omarchy", "shell", "javier.vmplc", "open"])
    root.close()
  }

  KeyboardPanel {
    id: panel

    anchorItem: root.anchorItem
    owner: root.hostWidget || root
    bar: root.bar
    open: root.opened
    centerOnBar: true
    contentWidth: panel.fittedContentWidth(Style.space(320))
    contentHeight: panel.fittedContentHeight(contentColumn.implicitHeight)

    Column {
      id: contentColumn

      width: parent.width
      spacing: Style.space(10)

      // ---- Header.
      Text {
        anchors.horizontalCenter: parent.horizontalCenter
        text: qsTr("Red PLC · VM")
        font.family: root.contentFontFamily
        font.pixelSize: Style.font.caption
        font.weight: Font.DemiBold
        color: Color.muted
      }

      // ---- Status.
      Item {
        width: parent.width
        height: statusColumn.implicitHeight

        Column {
          id: statusColumn

          anchors.centerIn: parent
          spacing: Style.space(4)

          Row {
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: Style.space(6)

            Text {
              text: root.host && root.host.configured ? "\uf0be" : "\uf083"
              renderType: Text.QtRendering
              font.family: "Material Symbols Rounded"
              font.pixelSize: Style.font.body
color: root.vmColor()
              anchors.verticalCenter: parent.verticalCenter
            }

            Text {
              text: root.host && root.host.configured
                ? qsTr("Puente listo") : qsTr("Sin configurar")
              font.family: root.contentFontFamily
              font.pixelSize: Style.font.body
              font.weight: Font.DemiBold
color: root.vmColor()
              anchors.verticalCenter: parent.verticalCenter
            }
          }

          Text {
            anchors.horizontalCenter: parent.horizontalCenter
            text: root.host && root.host.configured
              ? root.host.bridgeInfo
              : qsTr("Lanza la VM para configurarlo")
            font.family: root.contentFontFamily
            font.pixelSize: Style.font.caption
            color: Color.muted
            elide: Text.ElideMiddle
            width: panel.contentWidth - Style.space(60)
          }

          Row {
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: Style.space(6)

            Text {
              text: root.host && root.host.vmRunning ? "●" : "○"
              font.family: root.contentFontFamily
              font.pixelSize: Style.font.caption
              color: root.host && root.host.vmRunning
                ? Color.accent : Color.background
            }

            Text {
              text: root.host && root.host.vmRunning
                ? qsTr("VM en ejecución") : qsTr("VM detenida")
              font.family: root.contentFontFamily
              font.pixelSize: Style.font.caption
              color: Color.muted
            }
          }
        }
      }

      PanelSeparator {}

      // ---- Primary action.
      Button {
        anchors.horizontalCenter: parent.horizontalCenter
        text: root.host && root.host.vmRunning
          ? qsTr("Ir al escritorio 10")
          : qsTr("Abrir VM PLC")
        onClicked: root.activateVm()
      }

      Text {
        anchors.horizontalCenter: parent.horizontalCenter
        text: root.host && root.host.vmRunning
          ? qsTr("Ya está abierta; accede a la máquina.")
          : qsTr("vm-plc configura la red si hace falta.")
        font.family: root.contentFontFamily
        font.pixelSize: Style.font.caption
        color: Color.muted
      }

      // ---- Resultado de acciones (borrado).
      Text {
        anchors.horizontalCenter: parent.horizontalCenter
        width: panel.contentWidth - Style.space(60)
        text: root.actionStatus
        font.family: root.contentFontFamily
        font.pixelSize: Style.font.caption
        wrapMode: Text.WordWrap
        horizontalAlignment: Text.AlignHCenter
        color: root.actionUrgent
          ? (bar ? bar.urgent : Color.urgent) : Color.muted
        visible: root.actionStatus !== ""
      }

      // ---- Secondary actions.
      Row {
        anchors.horizontalCenter: parent.horizontalCenter
        spacing: Style.space(8)

        Button {
          text: qsTr("Asistente")
          tooltipText: qsTr("Abrir el configurador de red")
          onClicked: root.openAssistant()
        }

        Button {
          text: qsTr("Actualizar")
          tooltipText: qsTr("Releer el estado del puente y la VM")
          onClicked: if (root.host) root.host.refreshStatus()
        }

        Button {
          text: root.confirmDelete ? qsTr("¿Seguro?") : qsTr("Borrar red")
          tooltipText: qsTr("Eliminar puente, TAP e IP guardada")
          accent: root.confirmDelete ? Color.urgent : Color.accent
          onClicked: root.deleteBridge()
        }
      }
    }
  }
}