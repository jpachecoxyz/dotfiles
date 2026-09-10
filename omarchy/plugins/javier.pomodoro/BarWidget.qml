import QtQuick
import Quickshell
import Quickshell.Io
import qs.Commons
import qs.Ui

// Pomodoro timer for the bar.
//
// Left click opens the popup (status, transport controls, interval steppers,
// toggles). Middle click skips to the next phase and right click resets the
// cycle straight from the bar. After every focus block comes a short break;
// a long break lands once `longBreakInterval` focus blocks are done. Phase
// ends fire a desktop notification and auto-start the next phase unless
// `autoStart` is disabled.

BarWidget {
  id: root

  moduleName: "javier.pomodoro"

  readonly property int workMin: Math.max(1, setting("work", 25))
  readonly property int shortMin: Math.max(1, setting("shortBreak", 5))
  readonly property int longMin: Math.max(1, setting("longBreak", 15))
  readonly property int interval: Math.max(1, setting("longBreakInterval", 4))
  readonly property bool autoStart: setting("autoStart", true)
  readonly property bool notificationsEnabled: setting("notify", true)

  property string phase: "focus"
  property bool running: false
  property int completedFocus: 0
  property int remainingSec: workMin * 60

  readonly property string phaseIcon: phase === "focus" ? "\ue01b"
    : phase === "short" ? "\ue54e" : "\ue4fc"
  readonly property string phaseLabel: phase === "focus" ? qsTr("Enfoque")
    : phase === "short" ? qsTr("Descanso") : qsTr("Descanso largo")
  readonly property string timeText: displayTime()

  readonly property color ink: bar ? bar.urgent : Color.accent
  readonly property color runColor: phase === "focus"
    ? (bar ? bar.urgent : Color.urgent) : Color.accent
  readonly property color iconColor: running ? runColor : ink
  readonly property color textColor: running ? runColor : ink

  // ---- Popup contract (mirrors the clock widget): the bar identifies panels
  //      by the mounted widget, so open/close/opened live here.
  readonly property bool opened: panelLoader.item
    ? panelLoader.item.opened === true : false

  function open() {
    if (panelLoader.item) panelLoader.item.open()
  }

  function close() {
    if (panelLoader.item) panelLoader.item.close()
  }

  function togglePanel() {
    if (panelLoader.item) panelLoader.item.toggle()
  }

  function closeForPopoutSwitch() {
    if (panelLoader.item) panelLoader.item.closeForPopoutSwitch()
  }

  function injectPanel() {
    var target = panelLoader.item
    if (!target) return
    if ("bar" in target) target.bar = root.bar
    if ("settings" in target) target.settings = root.settings
    if ("anchorItem" in target) target.anchorItem = button
    if ("hostWidget" in target) target.hostWidget = root
  }

  onBarChanged: injectPanel()
  onSettingsChanged: {
    injectPanel()
    if (!running) remainingSec = durationFor(phase)
  }

  function durationFor(p) {
    if (p === "short") return shortMin * 60
    if (p === "long") return longMin * 60
    return workMin * 60
  }

  function displayTime() {
    var total = Math.max(0, remainingSec)
    var m = Math.floor(total / 60)
    var s = total % 60
    return (m < 10 ? "0" + m : "" + m) + ":" + (s < 10 ? "0" + s : "" + s)
  }

  // Local-apply first so the UI reacts instantly, then persist through the
  // shell's inline-entry writer; shell.json hot-reload echoes the same value.
  function applySettings(values) {
    var entry = { id: moduleName }
    for (var existing in settings) if (existing !== "id") entry[existing] = settings[existing]
    for (var key in values) entry[key] = values[key]
    settings = entry
    if (bar && bar.shell && typeof bar.shell.updateEntryInline === "function")
      bar.shell.updateEntryInline(moduleName, entry)
  }

  function toggle() {
    if (running) {
      running = false
      return
    }
    if (remainingSec <= 0) remainingSec = durationFor(phase)
    running = true
  }

  function reset() {
    phase = "focus"
    running = false
    completedFocus = 0
    remainingSec = durationFor(phase)
  }

  function skipPhase() { advance(autoStart) }

  function advance(startNext) {
    if (phase === "focus") {
      completedFocus++
      phase = (completedFocus % interval === 0) ? "long" : "short"
    } else {
      phase = "focus"
    }
    remainingSec = durationFor(phase)
    running = startNext
  }

  function completePhase() {
    var finished = phase
    advance(autoStart)
    if (!notificationsEnabled) return
    if (finished === "focus")
      notify(qsTr("Pomodoro"),
        phase === "long"
          ? qsTr("Varios bloques completados. Descanso largo.")
          : qsTr("Bloque terminado. Toma un descanso."))
    else
      notify(qsTr("Pomodoro"), qsTr("Descanso terminado. A enfocarse."))
  }

  function notify(title, body) {
    Quickshell.execDetached(["notify-send", "-a", "Pomodoro", title, body])
  }

  implicitWidth: button.implicitWidth
  implicitHeight: button.implicitHeight

  Timer {
    interval: 1000
    running: root.running
    repeat: true
    onTriggered: {
      root.remainingSec--
      if (root.remainingSec <= 0) root.completePhase()
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

  IpcHandler {
    target: "javier.pomodoro"

    function toggle(): void { root.broadcast("toggle") }
    function reset(): void { root.broadcast("reset") }
    function skip(): void { root.broadcast("skipPhase") }
    function open(): void { root.broadcast("open") }
    function status(): string {
      return root.phaseLabel + " " + root.timeText
        + (root.running ? "" : " (pausa)") + " [" + root.completedFocus + "]"
    }
  }

  WidgetButton {
    id: button

    anchors.fill: parent
    bar: root.bar
    text: root.timeText
    labelVisible: false
    horizontalMargin: 14
    tooltipText: root.phaseLabel + " • clic: menú • central: saltar • derecho: reiniciar"

    onPressed: function (b) {
      if (b === Qt.RightButton) root.broadcast("reset")
      else if (b === Qt.MiddleButton) root.broadcast("skipPhase")
      else root.togglePanel()
    }

    Row {
      anchors.centerIn: parent
      spacing: Style.space(6)

      Text {
        text: root.phaseIcon
        renderType: Text.QtRendering
        font.family: "Material Symbols Rounded"
        font.variableAxes: ({ "FILL": root.running ? 1 : 0 })
        font.pixelSize: Style.bar.iconFont
        color: root.iconColor
        anchors.verticalCenter: parent.verticalCenter
      }

      Text {
        text: root.timeText
        font.family: root.bar ? root.bar.fontFamily : Style.font.family
        font.pixelSize: Style.font.body
        color: root.textColor
        anchors.verticalCenter: parent.verticalCenter
      }
    }
  }
}
