package com.rarnu.command.emu.tool;

import android.view.KeyEvent;

public class TermKeyListener {

	private class ModifierKey {

		private int mState;

		private static final int UNPRESSED = 0;

		private static final int PRESSED = 1;

		private static final int RELEASED = 2;

		private static final int USED = 3;

		private static final int LOCKED = 4;

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
	}

	private ModifierKey mAltKey = new ModifierKey();

	private ModifierKey mCapKey = new ModifierKey();

	private ModifierKey mControlKey = new ModifierKey();

	public TermKeyListener() {
	}

	public void handleControlKey(boolean down) {
		if (down) {
			mControlKey.onPress();
		} else {
			mControlKey.onRelease();
		}
	}

	public int mapControlChar(int ch) {
		int result = ch;
		if (mControlKey.isActive()) {
			if (result >= 'a' && result <= 'z') {
				result = (char) (result - 'a' + '\001');
			} else if (result == ' ') {
				result = 0;
			} else if ((result == '[') || (result == '1')) {
				result = 27;
			} else if ((result == '\\') || (result == '.')) {
				result = 28;
			} else if ((result == ']') || (result == '0')) {
				result = 29;
			} else if ((result == '^') || (result == '6')) {
				result = 30; // control-^
			} else if ((result == '_') || (result == '5')) {
				result = 31;
			}
		}

		if (result > -1) {
			mAltKey.adjustAfterKeypress();
			mCapKey.adjustAfterKeypress();
			mControlKey.adjustAfterKeypress();
		}
		return result;
	}

	public int keyDown(int keyCode, KeyEvent event) {
		int result = -1;
		switch (keyCode) {
		case KeyEvent.KEYCODE_ALT_RIGHT:
		case KeyEvent.KEYCODE_ALT_LEFT:
			mAltKey.onPress();
			break;

		case KeyEvent.KEYCODE_SHIFT_LEFT:
		case KeyEvent.KEYCODE_SHIFT_RIGHT:
			mCapKey.onPress();
			break;

		case KeyEvent.KEYCODE_ENTER:

			result = '\r';
			break;

		case KeyEvent.KEYCODE_DEL:
			result = 127;
			break;

		default: {
			result = event
					.getUnicodeChar((mCapKey.isActive() ? KeyEvent.META_SHIFT_ON
							: 0)
							| (mAltKey.isActive() ? KeyEvent.META_ALT_ON : 0));
			break;
		}
		}

		result = mapControlChar(result);

		return result;
	}

	public void keyUp(int keyCode) {
		switch (keyCode) {
		case KeyEvent.KEYCODE_ALT_LEFT:
		case KeyEvent.KEYCODE_ALT_RIGHT:
			mAltKey.onRelease();
			break;
		case KeyEvent.KEYCODE_SHIFT_LEFT:
		case KeyEvent.KEYCODE_SHIFT_RIGHT:
			mCapKey.onRelease();
			break;
		default:
			break;
		}
	}
}