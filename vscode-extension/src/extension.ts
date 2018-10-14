'use strict';

import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';

interface LanguageConfiguration {
	vscodeName: string,
	miksiloName?: string,
}

export function activate(context: ExtensionContext) {
	
	const languages: Array<LanguageConfiguration> = [
		{ 
			vscodeName: "systemverilog", 
			miksiloName: "verilog"
		}]

	for(const language of languages) {
		activateLanguage(context, language)
	}
}

function activateLanguage(context: ExtensionContext, language: LanguageConfiguration) {

	const jar: string = workspace.getConfiguration('miksilo').get("jar") || process.env.MIKSILO;	
	language.miksiloName = language.miksiloName || language.vscodeName;

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
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
			// Notify the server about file changes to '.clientrc files contain in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		}
	}
	
	let disposable = new LanguageClient(
		'miksilo' + language.vscodeName, 
		language.vscodeName + " Miksilo", 
		serverOptions, clientOptions).start();
	
	context.subscriptions.push(disposable);
}
