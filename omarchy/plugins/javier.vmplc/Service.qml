import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs.Commons
import qs.Ui

Item {
    id: root

    property string omarchyPath: Quickshell.env("OMARCHY_PATH")
    property var shell: null
    property var manifest: null
    property var pluginRegistry: null

    property string vmScriptDir: Quickshell.env("HOME") + "/.local/bin/scripts"
    property string selectedIface: ""
    property var ifaces: []
    property bool busy: false
    property bool confirmDelete: false

    function showStatus(text, color) {
        statusText.text = text
        statusText.color = color ? color : Color.popups.text
    }

    function openPopup() {
        popup.visible = true
        root.busy = false
        root.confirmDelete = false
        ipInput.text = "192.168.0.10/24"
        root.showStatus("Introduce la IP del host sobre el puente y elige la interfaz del PLC.")
        ifaceProc.exec({
            command: [
                "/bin/sh",
                "-c",
                "ip -o link show 2>/dev/null | awk -F': ' '{print $2}' | sed 's/@.*//' | grep -vE '^(lo|br0|br-|docker|veth|virbr|tap)' | grep -vE '^wl'"
            ]
        })
        ipFocusTimer.restart()
    }

    function closePopup() {
        popup.visible = false
        root.busy = false
        root.confirmDelete = false
    }

    function runSetup() {
        if (root.busy) return
        root.confirmDelete = false
        var ipText = ipInput.text.trim()
        if (!/^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\/[0-9]+$/.test(ipText)) {
            root.showStatus("IP inválida (ej. 192.168.0.10/24)", Color.urgent)
            return
        }
        root.busy = true
        root.showStatus("Configurando red… se pedirá la contraseña.")
        setupProc.exec({
            command: [
                "pkexec",
                "env",
                "USER=" + Quickshell.env("USER"),
                "IFACE=" + root.selectedIface,
                vmScriptDir + "/vm-set-ip",
                ipText
            ]
        })
    }

    function showState() {
        if (root.busy) return
        root.confirmDelete = false
        root.busy = true
        root.showStatus("Obteniendo estado del puente…")
        showProc.exec({
            command: [
                vmScriptDir + "/vm-set-ip",
                "--show"
            ]
        })
    }

    function deleteBridge() {
        if (root.busy) return
        if (!root.confirmDelete) {
            root.confirmDelete = true
            root.showStatus("Eliminará el puente, el TAP y la IP guardada. Pulsa otra vez 'Borrar red'.", Color.urgent)
            return
        }
        root.confirmDelete = false
        root.busy = true
        root.showStatus("Eliminando puente y TAP…")
        delProc.exec({
            command: [
                "pkexec",
                "env",
                "USER=" + Quickshell.env("USER"),
                vmScriptDir + "/vm-set-ip",
                "--delete"
            ]
        })
    }

    IpcHandler {
        target: "javier.vmplc"
        function open(): string { root.openPopup(); return "ok" }
        function close(): string { root.closePopup(); return "ok" }
        function toggle(): string { popup.visible ? root.closePopup() : root.openPopup(); return "ok" }
        function ping(): string { return "ok" }
    }

    Timer {
        id: ipFocusTimer
        interval: 120
        onTriggered: ipInput.forceActiveFocus()
    }

    Process {
        id: ifaceProc
        stdout: StdioCollector {
            id: ifaceOut
            onStreamFinished: {
                var lines = ifaceOut.text.trim().split(/\s*\n\s*/)
                root.ifaces = []
                for (var i = 0; i < lines.length; i++) {
                    if (lines[i].length > 0) root.ifaces.push(lines[i])
                }
                if (root.selectedIface === "" && root.ifaces.length > 0) {
                    root.selectedIface = root.ifaces[0]
                }
            }
        }
    }

    Process {
        id: setupProc
        stdout: StdioCollector { id: setupOut }
        stderr: StdioCollector { id: setupErr }
        onExited: function(exitCode, exitStatus) {
            root.busy = false
            if (exitCode === 0) {
                root.showStatus("Red configurada. Lanzando vm-plc…", Color.accent)
                launchTimer.restart()
            } else {
                var raw = setupOut.text.trim() + (setupErr.text.trim() ? "\n" + setupErr.text.trim() : "")
                var lines = raw.split(/\s*\n\s*/)
                var relevant = lines.length > 0 ? lines[lines.length - 1] : ""
                if (!relevant) relevant = "Error (código " + exitCode + ")"
                root.showStatus(relevant, Color.urgent)
            }
        }
    }

    Process {
        id: showProc
        stdout: StdioCollector { id: showOut }
        stderr: StdioCollector { id: showErr }
        onExited: function(exitCode, exitStatus) {
            root.busy = false
            if (exitCode === 0) {
                var body = showOut.text.trim().replace(/\s*\n\s*/g, " · ")
                root.showStatus(body, Color.muted)
            } else {
                var lines = (showOut.text.trim() + "\n" + showErr.text.trim()).split(/\s*\n\s*/)
                var msg = lines[lines.length - 1] || ""
                root.showStatus(msg || ("Estado no disponible (código " + exitCode + ")"), Color.urgent)
            }
        }
    }

    Process {
        id: delProc
        stdout: StdioCollector { id: delOut }
        stderr: StdioCollector { id: delErr }
        onExited: function(exitCode, exitStatus) {
            root.busy = false
            if (exitCode === 0) {
                root.showStatus("Red eliminada. Al lanzar vm-plc se abrirá de nuevo este asistente.")
            } else {
                var lines = (delOut.text.trim() + "\n" + delErr.text.trim()).split(/\s*\n\s*/)
                var msg = lines[lines.length - 1] || ""
                root.showStatus(msg || ("Error (código " + exitCode + ")"), Color.urgent)
            }
        }
    }

    Timer {
        id: launchTimer
        interval: 1000
        onTriggered: {
            Quickshell.execDetached([root.vmScriptDir + "/vm-plc"])
            root.closePopup()
        }
    }

    PanelWindow {
        id: popup
        visible: false
        width: 350
        height: 300
        color: "transparent"
        focusable: true
        aboveWindows: true
        exclusionMode: ExclusionMode.Ignore
        WlrLayershell.namespace: "omarchy-vmplc"

        Rectangle {
            anchors.fill: parent
            color: Color.popups.background
            radius: Style.cornerRadius >= 0 ? Style.cornerRadius : 12
            border.color: Color.popups.border
            border.width: 1
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
                        color: Color.popups.text
                        font.family: Style.font.family
                        font.pixelSize: 13
                        font.bold: true
                    }

                    Button {
                        id: closeBtn
                        width: 22
                        height: 22
                        text: "✕"
                        onClicked: root.closePopup()
                    }
                }

                TextField {
                    id: ipInput
                    width: parent.width
                    height: 32
                    text: "192.168.0.10/24"
                }

                Text {
                    text: "Interfaz del puerto PLC:"
                    color: Color.popups.text
                    font.family: Style.font.family
                    font.pixelSize: 11
                    opacity: 0.75
                }

                Flow {
                    width: parent.width
                    spacing: 5
                    Repeater {
                        model: root.ifaces
                        Rectangle {
                            required property string modelData
                            width: ifaceLabel.implicitWidth + 14
                            height: 24
                            radius: 6
                            color: root.selectedIface === modelData ? Color.accent : Style.hoverFill
                            Text {
                                id: ifaceLabel
                                anchors.centerIn: parent
                                text: modelData
                                color: root.selectedIface === modelData ? Color.background : Color.foreground
                                font.family: Style.font.family
                                font.pixelSize: 11
                            }
                            MouseArea {
                                anchors.fill: parent
                                onClicked: {
                                    if (!root.busy) root.selectedIface = modelData
                                }
                            }
                        }
                    }
                }

                Text {
                    id: statusText
                    width: parent.width
                    height: 34
                    color: Color.popups.text
                    font.family: Style.font.family
                    font.pixelSize: 11
                    wrapMode: Text.WordWrap
                    verticalAlignment: Text.AlignTop
                    clip: true
                }

                Row {
                    width: parent.width
                    spacing: 6

                    Button {
                        id: delBtn
                        width: 92
                        text: root.confirmDelete ? "¿Seguro?" : "Borrar red"
                        accent: root.confirmDelete ? Color.urgent : Color.accent
                        enabled: !root.busy
                        onClicked: root.deleteBridge()
                    }

                    Button {
                        width: 72
                        text: "Ver estado"
                        enabled: !root.busy
                        onClicked: root.showState()
                    }
                }

                Row {
                    width: parent.width
                    spacing: 6
                    layoutDirection: Qt.RightToLeft

                    Button {
                        width: 170
                        height: 30
                        text: "Configurar y lanzar VM"
                        selected: true
                        enabled: !root.busy
                        onClicked: root.runSetup()
                    }

                    Button {
                        width: 76
                        height: 30
                        text: "Cancelar"
                        enabled: !root.busy
                        onClicked: root.closePopup()
                    }
                }
            }
        }
    }
}