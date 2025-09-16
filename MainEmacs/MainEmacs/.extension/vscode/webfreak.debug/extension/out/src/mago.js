"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const mibase_1 = require("./mibase");
const vscode_debugadapter_1 = require("vscode-debugadapter");
const mi2mago_1 = require("./backend/mi2/mi2mago");
class MagoDebugSession extends mibase_1.MI2DebugSession {
    constructor(debuggerLinesStartAt1, isServer = false) {
        super(debuggerLinesStartAt1, isServer);
    }
    initializeRequest(response, args) {
        response.body.supportsHitConditionalBreakpoints = true;
        response.body.supportsConfigurationDoneRequest = true;
        response.body.supportsConditionalBreakpoints = true;
        response.body.supportsFunctionBreakpoints = true;
        response.body.supportsEvaluateForHovers = true;
        this.sendResponse(response);
    }
    getThreadID() {
        return 0;
    }
    launchRequest(response, args) {
        this.miDebugger = new mi2mago_1.MI2_Mago(args.magomipath || "mago-mi", ["-q"], args.debugger_args, args.env);
        this.initDebugger();
        this.quit = false;
        this.attached = false;
        this.initialRunCommand = mibase_1.RunCommand.RUN;
        this.isSSH = false;
        this.started = false;
        this.crashed = false;
        this.setValuesFormattingMode(args.valuesFormatting);
        this.miDebugger.printCalls = !!args.printCalls;
        this.miDebugger.debugOutput = !!args.showDevDebugOutput;
        this.miDebugger.load(args.cwd, args.target, args.arguments, undefined, args.autorun || []).then(() => {
            this.sendResponse(response);
        }, err => {
            this.sendErrorResponse(response, 109, `Failed to load MI Debugger: ${err.toString()}`);
        });
    }
    attachRequest(response, args) {
        this.miDebugger = new mi2mago_1.MI2_Mago(args.magomipath || "mago-mi", [], args.debugger_args, args.env);
        this.initDebugger();
        this.quit = false;
        this.attached = true;
        this.initialRunCommand = args.stopAtConnect ? mibase_1.RunCommand.NONE : mibase_1.RunCommand.CONTINUE;
        this.isSSH = false;
        this.setValuesFormattingMode(args.valuesFormatting);
        this.miDebugger.printCalls = !!args.printCalls;
        this.miDebugger.debugOutput = !!args.showDevDebugOutput;
        this.miDebugger.attach(args.cwd, args.executable, args.target, args.autorun || []).then(() => {
            this.sendResponse(response);
        }, err => {
            this.sendErrorResponse(response, 110, `Failed to attach: ${err.toString()}`);
        });
    }
}
vscode_debugadapter_1.DebugSession.run(MagoDebugSession);
//# sourceMappingURL=mago.js.map