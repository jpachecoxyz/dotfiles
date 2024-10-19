import Widget from "resource:///com/github/Aylur/ags/widget.js";
import { execAsync } from "resource:///com/github/Aylur/ags/utils.js";
import Popup from "resource:///com/github/Aylur/ags/popup.js";

// Function to create individual power menu buttons
const PowerButton = (name, icon, command) => {
		return Widget.Button({
				on_clicked: () => execAsync(command),
				class_name: "power-button",
				child: Widget.Box({
						class_name: "power-button-container",
						vertical: true,
						children: [
								Widget.Icon({
										icon_name: icon,
										icon_size: 24,
								}),
								Widget.Label({
										label: name,
								}),
						],
				}),
		});
};

// PowerMenu Popup
const PowerMenuPopup = () => Popup({
		class_name: "power-menu-popup",
		anchor: ["top", "center"],
		children: [
				PowerButton("Lock", "system-lock-screen", ["hyprlock"]),
				PowerButton("Logout", "system-log-out", ["loginctl", "terminate-user", "$USER"]),
				PowerButton("Sleep", "system-suspend", ["systemctl", "suspend"]),
				PowerButton("Reboot", "system-reboot", ["systemctl", "reboot"]),
				PowerButton("Shutdown", "system-shutdown", ["systemctl", "poweroff"]),
		],
});

// Button in the bar that triggers the PowerMenuPopup
const PowerMenuTrigger = () => Widget.Button({
		class_name: "power-menu-trigger",
		child: Widget.Icon({
				icon_name: "system-shutdown", // You can change this to any preferred icon
				icon_size: 24,
		}),
		on_clicked: () => {
				// Toggle the visibility of the popup
				PowerMenuPopup().toggle();
		},
});

// Export the power menu trigger button for use in the bar
export default PowerMenuTrigger;
