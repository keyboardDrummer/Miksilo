'use strict';

import { workspace, ExtensionContext, window, Disposable } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';

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
    }
]

export function activate(context: ExtensionContext) {	
	workspace.onDidChangeConfiguration(() => activateJar(context));
	activateJar(context);
}

let previousJar: string | null | undefined = undefined;
function activateJar(context: ExtensionContext) {
	const jar: string = workspace.getConfiguration('miksilo').get("jar") || process.env.MIKSILO || null;
	if (jar === previousJar)
		return;
	previousJar = jar;

	if (!jar) {
		window.showErrorMessage("Could not locate a .jar for Miksilo. Please configure \"miksilo.jar\" in settings.");
		return;
	} 

	for(const previousClient of context.subscriptions) {
		previousClient.dispose()
	}
	context.subscriptions.length = 0;

	for(const language of languages) {
		const disposable = activateLanguage(jar, language);
		context.subscriptions.push(disposable);
	}
}

function activateLanguage(jar: string, language: LanguageConfiguration): Disposable {

	language.miksiloName = language.miksiloName || language.vscodeName;

	let serverOptions: ServerOptions = {
		command: "java",
		args: ["-jar",
			//"-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=6007",
			jar, 
			language.miksiloName]
	}
	
	let clientOptions: LanguageClientOptions = {
		documentSelector: [{scheme: 'file', language: language.vscodeName}],
		synchronize: {
			configurationSection: 'miksilo',
		}
	}
	
	return new LanguageClient(
		'miksilo' + language.vscodeName, 
		language.vscodeName + " Miksilo", 
		serverOptions, clientOptions).start();
}
