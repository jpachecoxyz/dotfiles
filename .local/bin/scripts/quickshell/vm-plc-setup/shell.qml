import QtQuick
import Quickshell
import Quickshell.Io

PanelWindow {
    id: win
    width: 340
    height: 296
    color: "transparent"
    focusable: true
    aboveWindows: true
    exclusionMode: ExclusionMode.Ignore
    visible: true

    property color bg: "#111c18"
    property color bgLight: "#23372B"
    property color accent: "#509475"
    property color fg: "#C1C497"
    property color fgDim: "#81B8A8"
    property color errColor: "#E67876"

    property string selectedIface: ""
    property var ifaces: []
    property bool busy: false
    property bool confirmDelete: false

    function showStatus(text, color) {
        statusText.text = text
        statusText.color = color ? color : win.fg
    }

    function runSetup() {
        if (win.busy) return
        win.confirmDelete = false
        var ipText = ipInput.text.trim()
        if (!/^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\/[0-9]+$/.test(ipText)) {
            win.showStatus("IP inválida (ej. 192.168.0.10/24)", win.errColor)
            return
        }
        win.busy = true
        ipInput.enabled = false
        win.showStatus("Configurando red… se pedirá la contraseña.")
        setupProc.exec({
            command: [
                "pkexec",
                "env",
                "USER=" + Quickshell.env("USER"),
                "IFACE=" + win.selectedIface,
                Quickshell.env("HOME") + "/.local/bin/scripts/vm-set-ip",
                ipText
            ]
        })
    }

    function showState() {
        if (win.busy) return
        win.confirmDelete = false
        win.busy = true
        win.showStatus("Obteniendo estado del puente…")
        showProc.exec({
            command: [
                Quickshell.env("HOME") + "/.local/bin/scripts/vm-set-ip",
                "--show"
            ]
        })
    }

    function deleteBridge() {
        if (win.busy) return
        if (!win.confirmDelete) {
            win.confirmDelete = true
            win.showStatus("Eliminará el puente, el TAP y la IP guardada. Pulsa otra vez 'Borrar red'.", win.errColor)
            return
        }
        win.confirmDelete = false
        win.busy = true
        win.showStatus("Eliminando puente y TAP…")
        delProc.exec({
            command: [
                "pkexec",
                "env",
                "USER=" + Quickshell.env("USER"),
                Quickshell.env("HOME") + "/.local/bin/scripts/vm-set-ip",
                "--delete"
            ]
        })
    }

    function quitApp() {
        Qt.exit(0)
    }

    Component.onCompleted: {
        ipInput.forceActiveFocus()
        ifaceProc.exec({
            command: [
                "/bin/sh",
                "-c",
                "ip -o link show 2>/dev/null | awk -F': ' '{print $2}' | sed 's/@.*//' | grep -vE '^(lo|br0|br-|docker|veth|virbr|tap)' | grep -vE '^wl'"
            ]
        })
    }

    Shortcut {
        sequence: "Escape"
        onActivated: win.quitApp()
    }

    Process {
        id: ifaceProc
        stdout: StdioCollector {
            id: ifaceOut
            onStreamFinished: {
                var lines = ifaceOut.text.trim().split(/\s*\n\s*/)
                win.ifaces = []
                for (var i = 0; i < lines.length; i++) {
                    if (lines[i].length > 0) win.ifaces.push(lines[i])
                }
                if (win.selectedIface === "" && win.ifaces.length > 0) {
                    win.selectedIface = win.ifaces[0]
                }
            }
        }
    }

    Process {
        id: setupProc
        stdout: StdioCollector { id: setupOut }
        stderr: StdioCollector { id: setupErr }
        onExited: function(exitCode, exitStatus) {
            win.busy = false
            ipInput.enabled = true
            if (exitCode === 0) {
                win.showStatus("Red configurada. Lanzando vm-plc…", win.accent)
                launchTimer.restart()
            } else {
                var raw = setupOut.text.trim() + (setupErr.text.trim() ? "\n" + setupErr.text.trim() : "")
                var lines = raw.split(/\s*\n\s*/)
                var relevant = lines.length > 0 ? lines[lines.length - 1] : ""
                if (!relevant) relevant = "Error (código " + exitCode + ")"
                win.showStatus(relevant, win.errColor)
            }
        }
    }

    Process {
        id: showProc
        stdout: StdioCollector { id: showOut }
        stderr: StdioCollector { id: showErr }
        onExited: function(exitCode, exitStatus) {
            win.busy = false
            if (exitCode === 0) {
                var body = showOut.text.trim().replace(/\s*\n\s*/g, " · ")
                win.showStatus(body, win.fgDim)
            } else {
                var lines = (showOut.text.trim() + "\n" + showErr.text.trim()).split(/\s*\n\s*/)
                var msg = lines[lines.length - 1] || ""
                win.showStatus(msg || ("Estado no disponible (código " + exitCode + ")"), win.errColor)
            }
        }
    }

    Process {
        id: delProc
        stdout: StdioCollector { id: delOut }
        stderr: StdioCollector { id: delErr }
        onExited: function(exitCode, exitStatus) {
            win.busy = false
            if (exitCode === 0) {
                win.showStatus("Red eliminada. Al lanzar vm-plc se abrirá de nuevo este asistente.")
            } else {
                var lines = (delOut.text.trim() + "\n" + delErr.text.trim()).split(/\s*\n\s*/)
                var msg = lines[lines.length - 1] || ""
                win.showStatus(msg || ("Error (código " + exitCode + ")"), win.errColor)
            }
        }
    }

    Process {
        id: vmProc
        command: ["vm-plc"]
    }

    Timer {
        id: launchTimer
        interval: 1000
        onTriggered: {
            vmProc.startDetached()
            win.quitApp()
        }
    }

    Rectangle {
        anchors.fill: parent
        color: win.bg
        radius: 10
        clip: true

        Column {
            anchors.fill: parent
            anchors.margins: 12
            spacing: 8

            Row {
                width: parent.width
                height: 22
                spacing: 6

                Text {
                    width: parent.width - closeBtn.width - parent.spacing
                    height: parent.height
                    verticalAlignment: Text.AlignVCenter
                    text: "Red PLC / VM"
                    color: win.fg
                    font.pixelSize: 13
                    font.bold: true
                }

                Rectangle {
                    id: closeBtn
                    width: 22
                    height: 22
                    radius: 6
                    color: win.bgLight
                    Text {
                        anchors.centerIn: parent
                        text: "✕"
                        color: win.fg
                        font.pixelSize: 11
                    }
                    MouseArea {
                        anchors.fill: parent
                        onClicked: win.quitApp()
                    }
                }
            }

            Rectangle {
                width: parent.width
                height: 30
                radius: 7
                color: win.bgLight
                border.color: ipInput.activeFocus ? win.accent : "transparent"
                border.width: 1
                TextInput {
                    id: ipInput
                    anchors.fill: parent
                    anchors.leftMargin: 10
                    anchors.rightMargin: 10
                    verticalAlignment: TextInput.AlignVCenter
                    text: "192.168.0.10/24"
                    color: win.fg
                    font.pixelSize: 12
                    selectByMouse: true
                }
            }

            Text {
                text: "Interfaz del puerto PLC:"
                color: win.fgDim
                font.pixelSize: 11
            }

            Flow {
                width: parent.width
                spacing: 5
                Repeater {
                    model: win.ifaces
                    Rectangle {
                        required property string modelData
                        width: ifaceLabel.implicitWidth + 14
                        height: 24
                        radius: 6
                        color: win.selectedIface === modelData ? win.accent : win.bgLight
                        Text {
                            id: ifaceLabel
                            anchors.centerIn: parent
                            text: modelData
                            color: win.selectedIface === modelData ? "#0b1512" : win.fg
                            font.pixelSize: 11
                        }
                        MouseArea {
                            anchors.fill: parent
                            onClicked: {
                                if (!win.busy) win.selectedIface = modelData
                            }
                        }
                    }
                }
            }

            Text {
                id: statusText
                width: parent.width
                height: 30
                color: win.fgDim
                font.pixelSize: 11
                wrapMode: Text.WordWrap
                verticalAlignment: Text.AlignTop
                clip: true
            }

            Row {
                width: parent.width
                spacing: 6

                Rectangle {
                    id: delBtn
                    width: 92
                    height: 26
                    radius: 7
                    color: win.confirmDelete ? win.errColor : win.bgLight
                    Text {
                        anchors.centerIn: parent
                        text: win.confirmDelete ? "¿Seguro?" : "Borrar red"
                        color: win.confirmDelete ? "#0b1512" : win.fg
                        font.pixelSize: 11
                    }
                    MouseArea {
                        enabled: !win.busy
                        anchors.fill: parent
                        onClicked: win.deleteBridge()
                    }
                }

                Rectangle {
                    id: statusBtn
                    width: 72
                    height: 26
                    radius: 7
                    color: win.bgLight
                    Text {
                        anchors.centerIn: parent
                        text: "Ver estado"
                        color: win.fg
                        font.pixelSize: 11
                    }
                    MouseArea {
                        enabled: !win.busy
                        anchors.fill: parent
                        onClicked: win.showState()
                    }
                }
            }

            Row {
                width: parent.width
                spacing: 6
                layoutDirection: Qt.RightToLeft

                Rectangle {
                    id: runBtn
                    width: 170
                    height: 30
                    radius: 7
                    color: win.busy ? Qt.darker(win.accent, 1.3) : win.accent
                    Text {
                        anchors.centerIn: parent
                        text: "Configurar y lanzar VM"
                        color: "#0b1512"
                        font.pixelSize: 12
                        font.bold: true
                    }
                    MouseArea {
                        enabled: !win.busy
                        anchors.fill: parent
                        onClicked: win.runSetup()
                    }
                }

                Rectangle {
                    id: cancelBtn
                    width: 76
                    height: 30
                    radius: 7
                    color: win.bgLight
                    Text {
                        anchors.centerIn: parent
                        text: "Cancelar"
                        color: win.fg
                        font.pixelSize: 12
                    }
                    MouseArea {
                        enabled: !win.busy
                        anchors.fill: parent
                        onClicked: win.quitApp()
                    }
                }
            }
        }
    }
}