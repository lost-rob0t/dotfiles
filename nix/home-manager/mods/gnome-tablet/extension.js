import GLib from 'gi://GLib';

import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import * as Main from 'resource:///org/gnome/shell/ui/main.js';

const SETTLE_DELAY_MS = 220;
const TERMINAL_RE = /(terminal|ptyxis|kgx|console|alacritty|kitty|foot|wezterm|xterm|urxvt|tilix|terminator|konsole)/i;

function isTerminal(window) {
    if (!window)
        return false;

    const values = [
        window.get_wm_class?.(),
        window.get_wm_class_instance?.(),
    ];

    return values.some(value => value && TERMINAL_RE.test(value));
}

export default class OskTerminalAvoidance extends Extension {
    enable() {
        this._keyboardActor = null;
        this._originalAnimateWindow = null;
        this._originalAffectsStruts = false;
        this._settleId = 0;

        this._keyboardVisibilityId = Main.keyboard.connect(
            'visibility-changed', () => this._sync());
        this._focusWindowId = global.display.connect(
            'notify::focus-window', () => this._sync());
        this._keyboardChildId = Main.layoutManager.keyboardBox.connect(
            'child-added', () => this._syncActor());

        this._syncActor();
        this._sync();
    }

    disable() {
        this._cancelSettle();

        if (this._keyboardVisibilityId)
            Main.keyboard.disconnect(this._keyboardVisibilityId);
        if (this._focusWindowId)
            global.display.disconnect(this._focusWindowId);
        if (this._keyboardChildId)
            Main.layoutManager.keyboardBox.disconnect(this._keyboardChildId);

        this._keyboardVisibilityId = 0;
        this._focusWindowId = 0;
        this._keyboardChildId = 0;

        this._restoreActor();
    }

    _syncActor() {
        const actor = Main.keyboard.keyboardActor;
        if (actor === this._keyboardActor)
            return;

        this._restoreActor();
        if (!actor)
            return;

        this._keyboardActor = actor;
        this._patchWindowAvoidance(actor);

        const actorData = this._getActorData(actor);
        if (actorData)
            this._originalAffectsStruts = actorData.affectsStruts;
    }

    _patchWindowAvoidance(actor) {
        if (typeof actor._animateWindow !== 'function')
            return;

        this._originalAnimateWindow = actor._animateWindow;
        const original = this._originalAnimateWindow;

        actor._animateWindow = function (window, show) {
            if (isTerminal(window))
                return;

            return original.call(this, window, show);
        };
    }

    _restoreActor() {
        if (!this._keyboardActor)
            return;

        const actorData = this._getActorData(this._keyboardActor);
        if (actorData) {
            actorData.affectsStruts = this._originalAffectsStruts;
            Main.layoutManager._queueUpdateRegions?.();
        }

        if (this._originalAnimateWindow)
            this._keyboardActor._animateWindow = this._originalAnimateWindow;

        this._keyboardActor = null;
        this._originalAnimateWindow = null;
        this._originalAffectsStruts = false;
    }

    _sync() {
        this._syncActor();

        const terminalFocused = isTerminal(global.display.focus_window);
        const reserve = Boolean(
            this._keyboardActor && Main.keyboard.visible && terminalFocused);

        this._setReserveSpace(reserve);

        if (reserve)
            this._scheduleSettledUpdate();
        else
            this._cancelSettle();
    }

    _setReserveSpace(enabled) {
        if (!this._keyboardActor)
            return;

        const actorData = this._getActorData(this._keyboardActor);
        if (!actorData)
            return;

        const desired = enabled ? true : this._originalAffectsStruts;
        if (actorData.affectsStruts === desired)
            return;

        actorData.affectsStruts = desired;
        Main.layoutManager._queueUpdateRegions?.();
    }

    _getActorData(actor) {
        const layout = Main.layoutManager;
        if (!layout?._trackedActors || typeof layout._findActor !== 'function')
            return null;

        const index = layout._findActor(actor);
        if (index < 0)
            return null;

        return layout._trackedActors[index];
    }

    _scheduleSettledUpdate() {
        this._cancelSettle();
        this._settleId = GLib.timeout_add(
            GLib.PRIORITY_DEFAULT,
            SETTLE_DELAY_MS,
            () => {
                this._settleId = 0;
                if (Main.keyboard.visible && isTerminal(global.display.focus_window))
                    Main.layoutManager._queueUpdateRegions?.();
                return GLib.SOURCE_REMOVE;
            });
    }

    _cancelSettle() {
        if (!this._settleId)
            return;

        GLib.source_remove(this._settleId);
        this._settleId = 0;
    }
}
