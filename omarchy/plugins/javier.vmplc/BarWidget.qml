import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui

// Bar pill for the PLC VM. Left click opens a dropdown with the bridge status
// and one button that launches the VM (or jumps to workspace 10 when it's
// already running). Bridge/VM state is polled every few seconds.

BarWidget {
  id: root

  moduleName: "javier.vmplc"

  property bool configured: false
  property string bridgeInfo: ""
  property bool vmRunning: false

  readonly property bool opened: panelLoader.item
    ? panelLoader.item.opened === true : false

  function open() { if (panelLoader.item) panelLoader.item.open() }
  function close() { if (panelLoader.item) panelLoader.item.close() }
  function togglePanel() { if (panelLoader.item) panelLoader.item.toggle() }

  function refreshStatus() {
    showProc.running = false
    showProc.running = true
    vmCheckProc.running = false
    vmCheckProc.running = true
  }

  function injectPanel() {
    var target = panelLoader.item
    if (!target) return
    if ("bar" in target) target.bar = root.bar
    if ("anchorItem" in target) target.anchorItem = button
    if ("hostWidget" in target) target.hostWidget = root
  }

  readonly property color iconColor: root.vmRunning
    ? (bar ? bar.urgent : Color.urgent)
    : root.configured
      ? (bar ? bar.foreground : Color.foreground)
      : Color.muted

  implicitWidth: button.implicitWidth
  implicitHeight: button.implicitHeight

  onBarChanged: injectPanel()
  onSettingsChanged: injectPanel()
  Component.onCompleted: Qt.callLater(function () { injectPanel(); refreshStatus() })

  Process {
    id: showProc

    command: [Quickshell.env("HOME") + "/.local/bin/scripts/vm-set-ip", "--show"]
    stdout: StdioCollector { id: showOut }

    onExited: function (exitCode) {
      root.configured = exitCode === 0
      root.bridgeInfo = showOut.text.trim().replace(/\s*\n\s*/g, "  ")
    }
  }

  Process {
    id: vmCheckProc

    command: ["pgrep", "-f", "^qemu-system-x86_64"]

    onExited: function (exitCode) {
      root.vmRunning = exitCode === 0
    }
  }

  Timer {
    interval: 7000
    repeat: true
    running: true
    onTriggered: root.refreshStatus()
  }

  IpcHandler {
    target: "javier.vmplc.bar"

    function open(): string { root.open(); return "ok" }
    function close(): string { root.close(); return "ok" }
    function toggle(): string { root.togglePanel(); return "ok" }
    function status(): string {
      return "bridge=" + (root.configured ? "ok" : "missing")
        + " vm=" + (root.vmRunning ? "running" : "stopped")
        + " panel=" + (root.opened ? "open" : "closed")
    }
  }

  Loader {
    id: panelLoader

    active: true
    source: Qt.resolvedUrl("Panel.qml")
    visible: false

    onLoaded: {
      root.injectPanel()
      Qt.callLater(root.injectPanel)
    }
  }

  BarIconButton {
    id: button

    anchors.fill: parent
    bar: root.bar
    text: "\ue30d"
    fontFamily: "Material Symbols Rounded"
    foreground: root.iconColor
    tooltipText: root.vmRunning ? qsTr("VM PLC en ejecución · clic: panel")
      : root.configured ? qsTr("Red PLC lista · clic: panel")
      : qsTr("Red PLC sin configurar · clic: panel")

    onPressed: function (b) {
      if (b === Qt.LeftButton) {
        root.refreshStatus()
        root.togglePanel()
      }
    }
  }
}