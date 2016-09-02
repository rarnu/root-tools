package com.rarnu.terminal.utils;

import android.view.KeyCharacterMap;
import android.view.KeyEvent;
import com.rarnu.terminal.renderer.TextRenderer;
import com.rarnu.terminal.session.TermSession;

import java.io.IOException;

public class TermKeyListener {

    public static final int KEYCODE_UNKNOWN = 0;
    public static final int KEYCODE_SOFT_LEFT = 1;
    public static final int KEYCODE_SOFT_RIGHT = 2;
    public static final int KEYCODE_HOME = 3;
    public static final int KEYCODE_BACK = 4;
    public static final int KEYCODE_CALL = 5;
    public static final int KEYCODE_ENDCALL = 6;
    public static final int KEYCODE_0 = 7;
    public static final int KEYCODE_1 = 8;
    public static final int KEYCODE_2 = 9;
    public static final int KEYCODE_3 = 10;
    public static final int KEYCODE_4 = 11;
    public static final int KEYCODE_5 = 12;
    public static final int KEYCODE_6 = 13;
    public static final int KEYCODE_7 = 14;
    public static final int KEYCODE_8 = 15;
    public static final int KEYCODE_9 = 16;
    public static final int KEYCODE_STAR = 17;
    public static final int KEYCODE_POUND = 18;
    public static final int KEYCODE_DPAD_UP = 19;
    public static final int KEYCODE_DPAD_DOWN = 20;
    public static final int KEYCODE_DPAD_LEFT = 21;
    public static final int KEYCODE_DPAD_RIGHT = 22;
    public static final int KEYCODE_DPAD_CENTER = 23;
    public static final int KEYCODE_VOLUME_UP = 24;
    public static final int KEYCODE_VOLUME_DOWN = 25;
    public static final int KEYCODE_POWER = 26;
    public static final int KEYCODE_CAMERA = 27;
    public static final int KEYCODE_CLEAR = 28;
    public static final int KEYCODE_A = 29;
    public static final int KEYCODE_B = 30;
    public static final int KEYCODE_C = 31;
    public static final int KEYCODE_D = 32;
    public static final int KEYCODE_E = 33;
    public static final int KEYCODE_F = 34;
    public static final int KEYCODE_G = 35;
    public static final int KEYCODE_H = 36;
    public static final int KEYCODE_I = 37;
    public static final int KEYCODE_J = 38;
    public static final int KEYCODE_K = 39;
    public static final int KEYCODE_L = 40;
    public static final int KEYCODE_M = 41;
    public static final int KEYCODE_N = 42;
    public static final int KEYCODE_O = 43;
    public static final int KEYCODE_P = 44;
    public static final int KEYCODE_Q = 45;
    public static final int KEYCODE_R = 46;
    public static final int KEYCODE_S = 47;
    public static final int KEYCODE_T = 48;
    public static final int KEYCODE_U = 49;
    public static final int KEYCODE_V = 50;
    public static final int KEYCODE_W = 51;
    public static final int KEYCODE_X = 52;
    public static final int KEYCODE_Y = 53;
    public static final int KEYCODE_Z = 54;
    public static final int KEYCODE_COMMA = 55;
    public static final int KEYCODE_PERIOD = 56;
    public static final int KEYCODE_ALT_LEFT = 57;
    public static final int KEYCODE_ALT_RIGHT = 58;
    public static final int KEYCODE_SHIFT_LEFT = 59;
    public static final int KEYCODE_SHIFT_RIGHT = 60;
    public static final int KEYCODE_TAB = 61;
    public static final int KEYCODE_SPACE = 62;
    public static final int KEYCODE_SYM = 63;
    public static final int KEYCODE_EXPLORER = 64;
    public static final int KEYCODE_ENVELOPE = 65;
    public static final int KEYCODE_ENTER = 66;
    public static final int KEYCODE_DEL = 67;
    public static final int KEYCODE_GRAVE = 68;
    public static final int KEYCODE_MINUS = 69;
    public static final int KEYCODE_EQUALS = 70;
    public static final int KEYCODE_LEFT_BRACKET = 71;
    public static final int KEYCODE_RIGHT_BRACKET = 72;
    public static final int KEYCODE_BACKSLASH = 73;
    public static final int KEYCODE_SEMICOLON = 74;
    public static final int KEYCODE_APOSTROPHE = 75;
    public static final int KEYCODE_SLASH = 76;
    public static final int KEYCODE_AT = 77;
    public static final int KEYCODE_NUM = 78;
    public static final int KEYCODE_HEADSETHOOK = 79;
    public static final int KEYCODE_FOCUS = 80;
    public static final int KEYCODE_PLUS = 81;
    public static final int KEYCODE_MENU = 82;
    public static final int KEYCODE_NOTIFICATION = 83;
    public static final int KEYCODE_SEARCH = 84;
    public static final int KEYCODE_MEDIA_PLAY_PAUSE = 85;
    public static final int KEYCODE_MEDIA_STOP = 86;
    public static final int KEYCODE_MEDIA_NEXT = 87;
    public static final int KEYCODE_MEDIA_PREVIOUS = 88;
    public static final int KEYCODE_MEDIA_REWIND = 89;
    public static final int KEYCODE_MEDIA_FAST_FORWARD = 90;
    public static final int KEYCODE_MUTE = 91;
    public static final int KEYCODE_PAGE_UP = 92;
    public static final int KEYCODE_PAGE_DOWN = 93;
    public static final int KEYCODE_PICTSYMBOLS = 94;
    public static final int KEYCODE_SWITCH_CHARSET = 95;
    public static final int KEYCODE_BUTTON_A = 96;
    public static final int KEYCODE_BUTTON_B = 97;
    public static final int KEYCODE_BUTTON_C = 98;
    public static final int KEYCODE_BUTTON_X = 99;
    public static final int KEYCODE_BUTTON_Y = 100;
    public static final int KEYCODE_BUTTON_Z = 101;
    public static final int KEYCODE_BUTTON_L1 = 102;
    public static final int KEYCODE_BUTTON_R1 = 103;
    public static final int KEYCODE_BUTTON_L2 = 104;
    public static final int KEYCODE_BUTTON_R2 = 105;
    public static final int KEYCODE_BUTTON_THUMBL = 106;
    public static final int KEYCODE_BUTTON_THUMBR = 107;
    public static final int KEYCODE_BUTTON_START = 108;
    public static final int KEYCODE_BUTTON_SELECT = 109;
    public static final int KEYCODE_BUTTON_MODE = 110;
    public static final int KEYCODE_ESCAPE = 111;
    public static final int KEYCODE_FORWARD_DEL = 112;
    public static final int KEYCODE_CTRL_LEFT = 113;
    public static final int KEYCODE_CTRL_RIGHT = 114;
    public static final int KEYCODE_CAPS_LOCK = 115;
    public static final int KEYCODE_SCROLL_LOCK = 116;
    public static final int KEYCODE_META_LEFT = 117;
    public static final int KEYCODE_META_RIGHT = 118;
    public static final int KEYCODE_FUNCTION = 119;
    public static final int KEYCODE_SYSRQ = 120;
    public static final int KEYCODE_BREAK = 121;
    public static final int KEYCODE_MOVE_HOME = 122;
    public static final int KEYCODE_MOVE_END = 123;
    public static final int KEYCODE_INSERT = 124;
    public static final int KEYCODE_FORWARD = 125;
    public static final int KEYCODE_MEDIA_PLAY = 126;
    public static final int KEYCODE_MEDIA_PAUSE = 127;
    public static final int KEYCODE_MEDIA_CLOSE = 128;
    public static final int KEYCODE_MEDIA_EJECT = 129;
    public static final int KEYCODE_MEDIA_RECORD = 130;
    public static final int KEYCODE_F1 = 131;
    public static final int KEYCODE_F2 = 132;
    public static final int KEYCODE_F3 = 133;
    public static final int KEYCODE_F4 = 134;
    public static final int KEYCODE_F5 = 135;
    public static final int KEYCODE_F6 = 136;
    public static final int KEYCODE_F7 = 137;
    public static final int KEYCODE_F8 = 138;
    public static final int KEYCODE_F9 = 139;
    public static final int KEYCODE_F10 = 140;
    public static final int KEYCODE_F11 = 141;
    public static final int KEYCODE_F12 = 142;
    public static final int KEYCODE_NUM_LOCK = 143;
    public static final int KEYCODE_NUMPAD_0 = 144;
    public static final int KEYCODE_NUMPAD_1 = 145;
    public static final int KEYCODE_NUMPAD_2 = 146;
    public static final int KEYCODE_NUMPAD_3 = 147;
    public static final int KEYCODE_NUMPAD_4 = 148;
    public static final int KEYCODE_NUMPAD_5 = 149;
    public static final int KEYCODE_NUMPAD_6 = 150;
    public static final int KEYCODE_NUMPAD_7 = 151;
    public static final int KEYCODE_NUMPAD_8 = 152;
    public static final int KEYCODE_NUMPAD_9 = 153;
    public static final int KEYCODE_NUMPAD_DIVIDE = 154;
    public static final int KEYCODE_NUMPAD_MULTIPLY = 155;
    public static final int KEYCODE_NUMPAD_SUBTRACT = 156;
    public static final int KEYCODE_NUMPAD_ADD = 157;
    public static final int KEYCODE_NUMPAD_DOT = 158;
    public static final int KEYCODE_NUMPAD_COMMA = 159;
    public static final int KEYCODE_NUMPAD_ENTER = 160;
    public static final int KEYCODE_NUMPAD_EQUALS = 161;
    public static final int KEYCODE_NUMPAD_LEFT_PAREN = 162;
    public static final int KEYCODE_NUMPAD_RIGHT_PAREN = 163;
    public static final int KEYCODE_VOLUME_MUTE = 164;
    public static final int KEYCODE_INFO = 165;
    public static final int KEYCODE_CHANNEL_UP = 166;
    public static final int KEYCODE_CHANNEL_DOWN = 167;
    public static final int KEYCODE_ZOOM_IN = 168;
    public static final int KEYCODE_ZOOM_OUT = 169;
    public static final int KEYCODE_TV = 170;
    public static final int KEYCODE_WINDOW = 171;
    public static final int KEYCODE_GUIDE = 172;
    public static final int KEYCODE_DVR = 173;
    public static final int KEYCODE_BOOKMARK = 174;
    public static final int KEYCODE_CAPTIONS = 175;
    public static final int KEYCODE_SETTINGS = 176;
    public static final int KEYCODE_TV_POWER = 177;
    public static final int KEYCODE_TV_INPUT = 178;
    public static final int KEYCODE_STB_POWER = 179;
    public static final int KEYCODE_STB_INPUT = 180;
    public static final int KEYCODE_AVR_POWER = 181;
    public static final int KEYCODE_AVR_INPUT = 182;
    public static final int KEYCODE_PROG_RED = 183;
    public static final int KEYCODE_PROG_GREEN = 184;
    public static final int KEYCODE_PROG_YELLOW = 185;
    public static final int KEYCODE_PROG_BLUE = 186;
    static public final int KEYCODE_OFFSET = 0xA00000;
    private final static boolean SUPPORT_8_BIT_META = false;
    private static final int META_ALT_ON = 2;
    private static final int META_CTRL_ON = 0x1000;
    private static final int META_CTRL_MASK = 0x7000;
    private String[] mKeyCodes = new String[256];
    private String[] mAppKeyCodes = new String[256];
    private ModifierKey mAltKey = new ModifierKey();
    private ModifierKey mCapKey = new ModifierKey();
    private ModifierKey mControlKey = new ModifierKey();
    private ModifierKey mFnKey = new ModifierKey();
    private int mCursorMode;
    private boolean mHardwareControlKey;
    private TermSession mTermSession;
    private int mBackKeyCode;
    private boolean mAltSendsEsc;
    private int mCombiningAccent;

    public TermKeyListener(TermSession termSession) {
        mTermSession = termSession;
        initKeyCodes();
        updateCursorMode();
    }

    private static int getCursorModeHelper(ModifierKey key, int shift) {
        return key.getUIMode() << shift;
    }

    public static boolean isEventFromToggleDevice(KeyEvent event) {
        KeyCharacterMap kcm = KeyCharacterMap.load(event.getDeviceId());
        return kcm.getModifierBehavior() == KeyCharacterMap.MODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED;
    }

    private void initKeyCodes() {
        mKeyCodes[KEYCODE_DPAD_CENTER] = "\015";
        mKeyCodes[KEYCODE_DPAD_UP] = "\033[A";
        mKeyCodes[KEYCODE_DPAD_DOWN] = "\033[B";
        mKeyCodes[KEYCODE_DPAD_RIGHT] = "\033[C";
        mKeyCodes[KEYCODE_DPAD_LEFT] = "\033[D";
        setFnKeys("vt100");
        mKeyCodes[KEYCODE_SYSRQ] = "\033[32~";
        mKeyCodes[KEYCODE_BREAK] = "\033[34~";

        mKeyCodes[KEYCODE_TAB] = "\011";
        mKeyCodes[KEYCODE_ENTER] = "\015";
        mKeyCodes[KEYCODE_ESCAPE] = "\033";

        mKeyCodes[KEYCODE_INSERT] = "\033[2~";
        mKeyCodes[KEYCODE_FORWARD_DEL] = "\033[3~";
        mKeyCodes[KEYCODE_MOVE_HOME] = "\033[1~";
        mKeyCodes[KEYCODE_MOVE_END] = "\033[4~";
        mKeyCodes[KEYCODE_PAGE_UP] = "\033[5~";
        mKeyCodes[KEYCODE_PAGE_DOWN] = "\033[6~";
        mKeyCodes[KEYCODE_DEL] = "\177";
        mKeyCodes[KEYCODE_NUM_LOCK] = "\033OP";
        mKeyCodes[KEYCODE_NUMPAD_DIVIDE] = "/";
        mKeyCodes[KEYCODE_NUMPAD_MULTIPLY] = "*";
        mKeyCodes[KEYCODE_NUMPAD_SUBTRACT] = "-";
        mKeyCodes[KEYCODE_NUMPAD_ADD] = "+";
        mKeyCodes[KEYCODE_NUMPAD_ENTER] = "\015";
        mKeyCodes[KEYCODE_NUMPAD_EQUALS] = "=";
        mKeyCodes[KEYCODE_NUMPAD_DOT] = ".";
        mKeyCodes[KEYCODE_NUMPAD_COMMA] = ",";
        mKeyCodes[KEYCODE_NUMPAD_0] = "0";
        mKeyCodes[KEYCODE_NUMPAD_1] = "1";
        mKeyCodes[KEYCODE_NUMPAD_2] = "2";
        mKeyCodes[KEYCODE_NUMPAD_3] = "3";
        mKeyCodes[KEYCODE_NUMPAD_4] = "4";
        mKeyCodes[KEYCODE_NUMPAD_5] = "5";
        mKeyCodes[KEYCODE_NUMPAD_6] = "6";
        mKeyCodes[KEYCODE_NUMPAD_7] = "7";
        mKeyCodes[KEYCODE_NUMPAD_8] = "8";
        mKeyCodes[KEYCODE_NUMPAD_9] = "9";

        mAppKeyCodes[KEYCODE_DPAD_UP] = "\033OA";
        mAppKeyCodes[KEYCODE_DPAD_DOWN] = "\033OB";
        mAppKeyCodes[KEYCODE_DPAD_RIGHT] = "\033OC";
        mAppKeyCodes[KEYCODE_DPAD_LEFT] = "\033OD";
        mAppKeyCodes[KEYCODE_NUMPAD_DIVIDE] = "\033Oo";
        mAppKeyCodes[KEYCODE_NUMPAD_MULTIPLY] = "\033Oj";
        mAppKeyCodes[KEYCODE_NUMPAD_SUBTRACT] = "\033Om";
        mAppKeyCodes[KEYCODE_NUMPAD_ADD] = "\033Ok";
        mAppKeyCodes[KEYCODE_NUMPAD_ENTER] = "\033OM";
        mAppKeyCodes[KEYCODE_NUMPAD_EQUALS] = "\033OX";
        mAppKeyCodes[KEYCODE_NUMPAD_DOT] = "\033On";
        mAppKeyCodes[KEYCODE_NUMPAD_COMMA] = "\033Ol";
        mAppKeyCodes[KEYCODE_NUMPAD_0] = "\033Op";
        mAppKeyCodes[KEYCODE_NUMPAD_1] = "\033Oq";
        mAppKeyCodes[KEYCODE_NUMPAD_2] = "\033Or";
        mAppKeyCodes[KEYCODE_NUMPAD_3] = "\033Os";
        mAppKeyCodes[KEYCODE_NUMPAD_4] = "\033Ot";
        mAppKeyCodes[KEYCODE_NUMPAD_5] = "\033Ou";
        mAppKeyCodes[KEYCODE_NUMPAD_6] = "\033Ov";
        mAppKeyCodes[KEYCODE_NUMPAD_7] = "\033Ow";
        mAppKeyCodes[KEYCODE_NUMPAD_8] = "\033Ox";
        mAppKeyCodes[KEYCODE_NUMPAD_9] = "\033Oy";
    }

    public void setBackKeyCharacter(int code) {
        mBackKeyCode = code;
    }

    public void handleHardwareControlKey(boolean down) {
        mHardwareControlKey = down;
    }

    public void handleControlKey(boolean down) {
        if (down) {
            mControlKey.onPress();
        } else {
            mControlKey.onRelease();
        }
        updateCursorMode();
    }

    public void handleFnKey(boolean down) {
        if (down) {
            mFnKey.onPress();
        } else {
            mFnKey.onRelease();
        }
        updateCursorMode();
    }

    public void setTermType(String termType) {
        setFnKeys(termType);
    }

    private void setFnKeys(String termType) {
        if (termType.equals("vt100")) {
            mKeyCodes[KEYCODE_F1] = "\033OP";
            mKeyCodes[KEYCODE_F2] = "\033OQ";
            mKeyCodes[KEYCODE_F3] = "\033OR";
            mKeyCodes[KEYCODE_F4] = "\033OS";
            mKeyCodes[KEYCODE_F5] = "\033Ot";
            mKeyCodes[KEYCODE_F6] = "\033Ou";
            mKeyCodes[KEYCODE_F7] = "\033Ov";
            mKeyCodes[KEYCODE_F8] = "\033Ol";
            mKeyCodes[KEYCODE_F9] = "\033Ow";
            mKeyCodes[KEYCODE_F10] = "\033Ox";
            mKeyCodes[KEYCODE_F11] = "\033[23~";
            mKeyCodes[KEYCODE_F12] = "\033[24~";
        } else if (termType.startsWith("linux")) {
            mKeyCodes[KEYCODE_F1] = "\033[[A";
            mKeyCodes[KEYCODE_F2] = "\033[[B";
            mKeyCodes[KEYCODE_F3] = "\033[[C";
            mKeyCodes[KEYCODE_F4] = "\033[[D";
            mKeyCodes[KEYCODE_F5] = "\033[[E";
            mKeyCodes[KEYCODE_F6] = "\033[17~";
            mKeyCodes[KEYCODE_F7] = "\033[18~";
            mKeyCodes[KEYCODE_F8] = "\033[19~";
            mKeyCodes[KEYCODE_F9] = "\033[20~";
            mKeyCodes[KEYCODE_F10] = "\033[21~";
            mKeyCodes[KEYCODE_F11] = "\033[23~";
            mKeyCodes[KEYCODE_F12] = "\033[24~";
        } else {

            mKeyCodes[KEYCODE_F1] = "\033OP";
            mKeyCodes[KEYCODE_F2] = "\033OQ";
            mKeyCodes[KEYCODE_F3] = "\033OR";
            mKeyCodes[KEYCODE_F4] = "\033OS";
            mKeyCodes[KEYCODE_F5] = "\033[15~";
            mKeyCodes[KEYCODE_F6] = "\033[17~";
            mKeyCodes[KEYCODE_F7] = "\033[18~";
            mKeyCodes[KEYCODE_F8] = "\033[19~";
            mKeyCodes[KEYCODE_F9] = "\033[20~";
            mKeyCodes[KEYCODE_F10] = "\033[21~";
            mKeyCodes[KEYCODE_F11] = "\033[23~";
            mKeyCodes[KEYCODE_F12] = "\033[24~";
        }
    }

    public int mapControlChar(int ch) {
        return mapControlChar(mHardwareControlKey || mControlKey.isActive(), mFnKey.isActive(), ch);
    }

    public int mapControlChar(boolean control, boolean fn, int ch) {
        int result = ch;
        if (control) {
            if (result >= 'a' && result <= 'z') {
                result = (char) (result - 'a' + '\001');
            } else if (result >= 'A' && result <= 'Z') {
                result = (char) (result - 'A' + '\001');
            } else if (result == ' ' || result == '2') {
                result = 0;
            } else if (result == '[' || result == '3') {
                result = 27;
            } else if (result == '\\' || result == '4') {
                result = 28;
            } else if (result == ']' || result == '5') {
                result = 29;
            } else if (result == '^' || result == '6') {
                result = 30;
            } else if (result == '_' || result == '7') {
                result = 31;
            } else if (result == '8') {
                result = 127;
            } else if (result == '9') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F11;
            } else if (result == '0') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F12;
            }
        } else if (fn) {
            if (result == 'w' || result == 'W') {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_UP;
            } else if (result == 'a' || result == 'A') {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_LEFT;
            } else if (result == 's' || result == 'S') {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_DOWN;
            } else if (result == 'd' || result == 'D') {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_RIGHT;
            } else if (result == 'p' || result == 'P') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_PAGE_UP;
            } else if (result == 'n' || result == 'N') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_PAGE_DOWN;
            } else if (result == 't' || result == 'T') {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_TAB;
            } else if (result == 'l' || result == 'L') {
                result = '|';
            } else if (result == 'u' || result == 'U') {
                result = '_';
            } else if (result == 'e' || result == 'E') {
                result = 27;
            } else if (result == '.') {
                result = 28;
            } else if (result > '0' && result <= '9') {
                result = (char) (result + KEYCODE_OFFSET + TermKeyListener.KEYCODE_F1 - 1);
            } else if (result == '0') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F10;
            } else if (result == 'i' || result == 'I') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_INSERT;
            } else if (result == 'x' || result == 'X') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_FORWARD_DEL;
            } else if (result == 'h' || result == 'H') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_MOVE_HOME;
            } else if (result == 'f' || result == 'F') {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_MOVE_END;
            }
        }

        if (result > -1) {
            mAltKey.adjustAfterKeypress();
            mCapKey.adjustAfterKeypress();
            mControlKey.adjustAfterKeypress();
            mFnKey.adjustAfterKeypress();
            updateCursorMode();
        }

        return result;
    }

    public void keyDown(int keyCode, KeyEvent event, boolean appMode, boolean allowToggle) throws IOException {
        if (handleKeyCode(keyCode, appMode)) {
            return;
        }
        int result = -1;
        boolean chordedCtrl = false;
        boolean setHighBit = false;
        switch (keyCode) {
            case KeyEvent.KEYCODE_ALT_RIGHT:
            case KeyEvent.KEYCODE_ALT_LEFT:
                if (allowToggle) {
                    mAltKey.onPress();
                    updateCursorMode();
                }
                break;

            case KeyEvent.KEYCODE_SHIFT_LEFT:
            case KeyEvent.KEYCODE_SHIFT_RIGHT:
                if (allowToggle) {
                    mCapKey.onPress();
                    updateCursorMode();
                }
                break;

            case KEYCODE_CTRL_LEFT:
            case KEYCODE_CTRL_RIGHT:
            case KEYCODE_CAPS_LOCK:
            case KEYCODE_FUNCTION:
                return;

            case KeyEvent.KEYCODE_BACK:
                result = mBackKeyCode;
                break;

            default: {
                int metaState = event.getMetaState();
                chordedCtrl = ((META_CTRL_ON & metaState) != 0);
                boolean effectiveCaps = allowToggle && (mCapKey.isActive());
                boolean effectiveAlt = allowToggle && mAltKey.isActive();
                int effectiveMetaState = metaState & (~META_CTRL_MASK);
                if (effectiveCaps) {
                    effectiveMetaState |= KeyEvent.META_SHIFT_ON;
                }
                if (!allowToggle && (effectiveMetaState & META_ALT_ON) != 0) {
                    effectiveAlt = true;
                }
                if (effectiveAlt) {
                    if (mAltSendsEsc) {
                        mTermSession.write(new byte[]{0x1b}, 0, 1);
                        effectiveMetaState &= ~KeyEvent.META_ALT_MASK;
                    } else if (SUPPORT_8_BIT_META) {
                        setHighBit = true;
                        effectiveMetaState &= ~KeyEvent.META_ALT_MASK;
                    } else {
                        effectiveMetaState |= KeyEvent.META_ALT_ON;
                    }
                }

                if ((metaState & KeyEvent.META_META_ON) != 0) {
                    if (mAltSendsEsc) {
                        mTermSession.write(new byte[]{0x1b}, 0, 1);
                        effectiveMetaState &= ~KeyEvent.META_META_MASK;
                    } else {
                        if (SUPPORT_8_BIT_META) {
                            setHighBit = true;
                            effectiveMetaState &= ~KeyEvent.META_META_MASK;
                        }
                    }
                }
                result = event.getUnicodeChar(effectiveMetaState);

                if ((result & KeyCharacterMap.COMBINING_ACCENT) != 0) {

                    mCombiningAccent = result & KeyCharacterMap.COMBINING_ACCENT_MASK;
                    return;
                }
                if (mCombiningAccent != 0) {
                    int unaccentedChar = result;
                    result = KeyCharacterMap.getDeadChar(mCombiningAccent, unaccentedChar);

                    mCombiningAccent = 0;
                }

                break;
            }
        }

        boolean effectiveControl = chordedCtrl || mHardwareControlKey || (allowToggle && mControlKey.isActive());
        boolean effectiveFn = allowToggle && mFnKey.isActive();

        result = mapControlChar(effectiveControl, effectiveFn, result);

        if (result >= KEYCODE_OFFSET) {
            handleKeyCode(result - KEYCODE_OFFSET, appMode);
        } else if (result >= 0) {
            if (setHighBit) {
                result |= 0x80;
            }
            mTermSession.write(result);
        }
    }

    public int getCombiningAccent() {
        return mCombiningAccent;
    }

    public int getCursorMode() {
        return mCursorMode;
    }

    private void updateCursorMode() {
        mCursorMode = getCursorModeHelper(mCapKey, TextRenderer.MODE_SHIFT_SHIFT) | getCursorModeHelper(mAltKey, TextRenderer.MODE_ALT_SHIFT) | getCursorModeHelper(mControlKey, TextRenderer.MODE_CTRL_SHIFT) | getCursorModeHelper(mFnKey, TextRenderer.MODE_FN_SHIFT);
    }

    public boolean handleKeyCode(int keyCode, boolean appMode)
            throws IOException {
        if (keyCode >= 0 && keyCode < mKeyCodes.length) {
            String code = null;
            if (appMode) {
                code = mAppKeyCodes[keyCode];
            }
            if (code == null) {
                code = mKeyCodes[keyCode];
            }
            if (code != null) {
                mTermSession.write(code);
                return true;
            }
        }
        return false;
    }

    public void keyUp(int keyCode, KeyEvent event) {
        boolean allowToggle = isEventFromToggleDevice(event);
        switch (keyCode) {
            case KeyEvent.KEYCODE_ALT_LEFT:
            case KeyEvent.KEYCODE_ALT_RIGHT:
                if (allowToggle) {
                    mAltKey.onRelease();
                    updateCursorMode();
                }
                break;
            case KeyEvent.KEYCODE_SHIFT_LEFT:
            case KeyEvent.KEYCODE_SHIFT_RIGHT:
                if (allowToggle) {
                    mCapKey.onRelease();
                    updateCursorMode();
                }
                break;

            case KEYCODE_CTRL_LEFT:
            case KEYCODE_CTRL_RIGHT:
                break;

            default:
                break;
        }
    }

    public boolean getAltSendsEsc() {
        return mAltSendsEsc;
    }

    public void setAltSendsEsc(boolean flag) {
        mAltSendsEsc = flag;
    }

    public boolean isAltActive() {
        return mAltKey.isActive();
    }

    public boolean isCtrlActive() {
        return mControlKey.isActive();
    }
}
