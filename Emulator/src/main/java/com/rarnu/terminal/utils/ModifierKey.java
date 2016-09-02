package com.rarnu.terminal.utils;

import com.rarnu.terminal.renderer.TextRenderer;

class ModifierKey {

    private static final int UNPRESSED = 0;
    private static final int PRESSED = 1;
    private static final int RELEASED = 2;
    private static final int USED = 3;
    private static final int LOCKED = 4;
    private int mState;

    public ModifierKey() {
        mState = UNPRESSED;
    }

    public void onPress() {
        switch (mState) {
            case PRESSED:
                break;
            case RELEASED:
                mState = LOCKED;
                break;
            case USED:
                break;
            case LOCKED:
                mState = UNPRESSED;
                break;
            default:
                mState = PRESSED;
                break;
        }
    }

    public void onRelease() {
        switch (mState) {
            case USED:
                mState = UNPRESSED;
                break;
            case PRESSED:
                mState = RELEASED;
                break;
            default:
                break;
        }
    }

    public void adjustAfterKeypress() {
        switch (mState) {
            case PRESSED:
                mState = USED;
                break;
            case RELEASED:
                mState = UNPRESSED;
                break;
            default:
                break;
        }
    }

    public boolean isActive() {
        return mState != UNPRESSED;
    }

    public int getUIMode() {
        switch (mState) {
            default:
            case UNPRESSED:
                return TextRenderer.MODE_OFF;
            case PRESSED:
            case RELEASED:
            case USED:
                return TextRenderer.MODE_ON;
            case LOCKED:
                return TextRenderer.MODE_LOCKED;
        }
    }
}