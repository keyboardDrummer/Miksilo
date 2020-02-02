'use strict';

import { workspace, ExtensionContext, window, Disposable } from 'vscode';
import { Executable, LanguageClient, LanguageClientOptions, ServerOptions, ExecutableOptions } from 'vscode-languageclient';
import * as fs from 'fs'

interface LanguageConfiguration {
	vscodeName: string,
	miksiloName?: string,
}
const languages: Array<LanguageConfiguration> = [
	{ 
		vscodeName: "systemverilog", 
		miksiloName: "verilog"
	},
	{
        vscodeName: "solidity",
        miksiloName: "solidity"
    },
    {
         vscodeName: "java",
         miksiloName: "java"
    },
    {
         vscodeName: "cloudFormation",
         miksiloName: "cloudFormation"
    },
    {
         vscodeName: "yamlCloudFormation",
         miksiloName: "yamlCloudFormation"
    },
    {
         vscodeName: "smithy",
         miksiloName: "smithy"
    }
]

export function activate(context: ExtensionContext) {

	workspace.onDidChangeConfiguration(() => activateJar(context));
	activateJar(context);
}

abstract class Mode {
    constructor(readonly reason: string) {}
    abstract setupExecutable(executable: Executable): void
}

class JVMMode extends Mode {
    constructor(readonly jar: string, reason: string = "") {
        super(reason);
    }
    setupExecutable(executable: Executable): void {
    	executable.command = "java";

	    executable.args = ["-jar", this.jar]
    }

    toString() {
        return "JVM with jar " + this.jar;
    }
}

class JSMode extends Mode {
    constructor(readonly program: string, reason: string = "") {
        super(reason);
    }

    setupExecutable(executable: Executable): void {
	    executable.command = "node";

	    executable.args = [this.program]
    }

    toString() {
        return "Node with program " + this.program;
    }
}

function getMode(): Mode | undefined {
    if (process.env.MIKSILO) {
        return new JVMMode(process.env.MIKSILO, "JVM language server passed in environment variable MIKSILO");
    }

    if (process.env.JSMIKSILO) {
        return new JSMode(process.env.JSMIKSILO, "Node language server passed in environment variable JSMIKSILO");
    }

	const settingsJar: string = workspace.getConfiguration('miksilo').get("jar")
	if (settingsJar) {
	    return new JVMMode(settingsJar, "Miksilo jar settings specified");
	}
	const jar: string = `${__dirname}/LanguageServer.jar`;
	if (fs.existsSync(jar)) {
	    return new JVMMode(jar, `Found ${jar}`);
	}

	const nodeProgram: string = workspace.getConfiguration('miksilo').get("js") || `${__dirname}/LanguageServer.js`
	if (nodeProgram) {
	    return new JSMode(nodeProgram)
	}
	return undefined
}

let previousMode: Mode | undefined = undefined;
function activateJar(context: ExtensionContext) {

	const mode = getMode()
	if (mode === previousMode)
		return;
	previousMode = mode;
	if (!mode) {
		window.showErrorMessage("Could not locate a language server. Please configure \"miksilo.jar\" in settings.");
		return;
	}

	for(const previousClient of context.subscriptions) {
		previousClient.dispose()
	}
	context.subscriptions.length = 0;

	for(const language of languages) {
		const disposable = activateLanguage(mode, language);
		context.subscriptions.push(disposable);
	}
}

function activateLanguage(mode: Mode, language: LanguageConfiguration): Disposable {

	let serverOptions: ServerOptions = prepareExecutable(mode, language)

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: language.vscodeName}],
		synchronize: {
			configurationSection: 'miksilo',
		}
	}

	const start = Date.now()
	const languageClient = new LanguageClient(
		'miksilo' + language.vscodeName,
		language.vscodeName + " Miksilo",
		serverOptions, clientOptions);

	const info = (message: String) => {
		languageClient.outputChannel.appendLine(`[INFO] ${message}`);
	}
	info(mode.reason);
	languageClient.onReady().then(_ => {
		const connectionTime = Date.now() - start;
		info(`Connection time was ${connectionTime}`);
	})
	languageClient.onTelemetry((data: any) => {
		const {name, value} = data
		info(`${name} was ${value}`);
	})

	info("Using Miksilo mode " + mode);
	return languageClient.start();
}

function prepareExecutable(mode: Mode, language: LanguageConfiguration): Executable {

	const executable: Executable = Object.create(null);
	const options: ExecutableOptions = Object.create(null);
	options.env = process.env;
	options.stdio = 'pipe';
	executable.options = options;
	mode.setupExecutable(executable);

	language.miksiloName = language.miksiloName || language.vscodeName;
	executable.args.push(language.miksiloName)
	return executable;
}

export function deactivate() {
}