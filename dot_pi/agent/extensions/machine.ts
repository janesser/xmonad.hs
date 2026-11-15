import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

/**
 * Machine controls for the fleet — exposed to the agent as tools so a Signal
 * message like "wake up cyberkleiber" is handled without the agent guessing
 * command names. All scripts are chezmoi-managed and only run with the agent's
 * own permissions (signal-pi.service runs as jan).
 *
 * The agent runs in signal-pi.service, so `pi.exec` calls are plain `bash -c`
 * with absolute script paths — `~/.local/bin/wakeup.cyberkleiber` is not in
 * that unit's PATH by default and must be added there (see the unit change).
 */
export default function (pi: ExtensionAPI) {
	console.log("[machine] extension loaded");

	const WAKE_SCRIPT = "/home/jan/.local/bin/wakeup.cyberkleiber";

	/** Run one command and report whether it left the target up. */
	async function run(command: string): Promise<string> {
		const result = await pi.exec("bash", ["-c", `${command} && echo "up"`], { timeout: 30_000 });
		return result.stdout.trim().endsWith("up") ? "up" : "down";
	}

	pi.registerTool({
		name: "machine_wake",
		label: "Wake machine",
		description:
			"Send Wake-on-LAN to cyberkleiber and poll until it answers, so callers never race a box that is still booting.",
		parameters: Type.Object({}),
		async execute(_toolCallId, _params) {
			const state = await run(`${WAKE_SCRIPT}`);
			return { content: [{ type: "text", text: `cyberkleiber is ${state}` }], details: {} };
		},
	});

	pi.registerTool({
		name: "machine_status",
		label: "Machine status",
		description: "Check whether cyberkleiber answers to ping.",
		parameters: Type.Object({}),
		async execute(_toolCallId, _params) {
			const state = await run(`ping -c1 -W5 cyberkleiber`);
			return { content: [{ type: "text", text: `cyberkleiber is ${state}` }], details: {} };
		},
	});

	pi.registerTool({
		name: "machine_sleep",
		label: "Sleep machine",
		description:
			"Put cyberkleiber to sleep (systemctl --user suspend). Needs a user console session on the box.",
		parameters: Type.Object({}),
		async execute(_toolCallId, _params) {
			const result = await pi.exec("bash", ["-c", `ssh jan@cyberkleiber "systemctl --user suspend"`]);
			return {
				content: [
					{
						type: "text",
						text: result.code === 0 ? "cyberkleiber is asleep" : "could not sleep cyberkleiber (no seat or ssh failed)",
					},
				],
				details: {},
			};
		},
	});
}
