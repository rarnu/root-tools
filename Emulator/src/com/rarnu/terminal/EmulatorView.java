package com.rarnu.terminal;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.view.GestureDetector;
import android.view.KeyEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.inputmethod.*;
import android.widget.Scroller;
import com.rarnu.terminal.callback.UpdateCallback;
import com.rarnu.terminal.renderer.BaseTextRenderer;
import com.rarnu.terminal.renderer.PaintRenderer;
import com.rarnu.terminal.renderer.TextRenderer;
import com.rarnu.terminal.screen.TranscriptScreen;
import com.rarnu.terminal.session.TermSession;
import com.rarnu.terminal.utils.ColorScheme;
import com.rarnu.terminal.utils.TermKeyListener;

import java.io.IOException;

public class EmulatorView extends View implements GestureDetector.OnGestureListener {

    private static final int CURSOR_BLINK_PERIOD = 1000;
    private static final int SELECT_TEXT_OFFSET_Y = -40;
    private final static boolean sTrapAltAndMeta = Build.MODEL.contains("Transformer TF101");
    private final Handler mHandler = new Handler();
    private boolean mKnownSize;
    private boolean mDeferInit = false;
    private int mVisibleWidth;
    private int mVisibleHeight;
    private TermSession mTermSession;
    private TranscriptScreen mTranscriptScreen;
    private float mCharacterWidth;
    private int mCharacterHeight;
    private int mTopOfScreenMargin;
    private TextRenderer mTextRenderer;
    private int mTextSize = 10;
    private int mCursorBlink;
    private ColorScheme mColorScheme = BaseTextRenderer.defaultColorScheme;
    private Paint mForegroundPaint;
    private Paint mBackgroundPaint;
    private boolean mUseCookedIme;
    private TerminalEmulator mEmulator;
    private int mRows;
    private int mColumns;
    private int mVisibleColumns;
    private int mTopRow;
    private int mLeftColumn;
    private boolean mCursorVisible = true;
    private boolean mIsSelectingText = false;
    private boolean mBackKeySendsCharacter = false;
    private int mControlKeyCode;
    private int mFnKeyCode;
    private boolean mIsControlKeySent = false;
    private boolean mIsFnKeySent = false;
    private float mDensity;
    private float mScaledDensity;
    private int mSelXAnchor = -1;
    private int mSelYAnchor = -1;
    private int mSelX1 = -1;
    private int mSelY1 = -1;
    private int mSelX2 = -1;
    private int mSelY2 = -1;
    private Runnable mBlinkCursor = new Runnable() {
        public void run() {
            if (mCursorBlink != 0) {
                mCursorVisible = !mCursorVisible;
                mHandler.postDelayed(this, CURSOR_BLINK_PERIOD);
            } else {
                mCursorVisible = true;
            }
            invalidate();
        }
    };
    private GestureDetector mGestureDetector;
    private GestureDetector.OnGestureListener mExtGestureListener;
    private Scroller mScroller;
    private Runnable mFlingRunner = new Runnable() {
        public void run() {
            if (mScroller.isFinished()) {
                return;
            }

            boolean more = mScroller.computeScrollOffset();
            int newTopRow = mScroller.getCurrY();
            if (newTopRow != mTopRow) {
                mTopRow = newTopRow;
                invalidate();
            }

            if (more) {
                post(this);
            }

        }
    };
    private float mScrollRemainder;
    private TermKeyListener mKeyListener;
    private String mImeBuffer = "";
    private UpdateCallback mUpdateNotify = new UpdateCallback() {
        public void onUpdate() {
            if (mIsSelectingText) {
                int rowShift = mEmulator.getScrollCounter();
                mSelY1 -= rowShift;
                mSelY2 -= rowShift;
                mSelYAnchor -= rowShift;
            }
            mEmulator.clearScrollCounter();
            ensureCursorVisible();
            invalidate();
        }
    };

    public EmulatorView(Context context, TermSession session,
                        DisplayMetrics metrics) {
        super(context);
        attachSession(session);
        setDensity(metrics);
        commonConstructor(context);
    }

    public EmulatorView(Context context, AttributeSet attrs) {
        super(context, attrs);
        commonConstructor(context);
    }

    public EmulatorView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        commonConstructor(context);
    }

    private void commonConstructor(Context context) {
        mScroller = new Scroller(context);
    }

    public void attachSession(TermSession session) {
        mTextRenderer = null;
        mForegroundPaint = new Paint();
        mBackgroundPaint = new Paint();
        mTopRow = 0;
        mLeftColumn = 0;
        mGestureDetector = new GestureDetector(getContext(), this);
        setVerticalScrollBarEnabled(true);
        setFocusable(true);
        setFocusableInTouchMode(true);

        mTermSession = session;

        mKeyListener = new TermKeyListener(session);

        if (mDeferInit) {
            mDeferInit = false;
            mKnownSize = true;
            initialize();
        }
    }

    public void setDensity(DisplayMetrics metrics) {
        if (mDensity == 0) {
            mTextSize = (int) (mTextSize * metrics.density);
        }
        mDensity = metrics.density;
        mScaledDensity = metrics.scaledDensity;
    }

    public void onResume() {
        updateSize(false);
        if (mCursorBlink != 0) {
            mHandler.postDelayed(mBlinkCursor, CURSOR_BLINK_PERIOD);
        }
    }

    public void onPause() {
        if (mCursorBlink != 0) {
            mHandler.removeCallbacks(mBlinkCursor);
        }
    }

    public void setColorScheme(ColorScheme scheme) {
        if (scheme == null) {
            mColorScheme = BaseTextRenderer.defaultColorScheme;
        } else {
            mColorScheme = scheme;
        }
        updateText();
    }

    @Override
    public boolean onCheckIsTextEditor() {
        return true;
    }

    @Override
    public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
        outAttrs.inputType = mUseCookedIme ? EditorInfo.TYPE_CLASS_TEXT : EditorInfo.TYPE_NULL;
        return new BaseInputConnection(this, true) {
            private int mCursor;
            private int mComposingTextStart;
            private int mComposingTextEnd;
            private int mSelectedTextStart;
            private int mSelectedTextEnd;

            private void sendText(CharSequence text) {
                int n = text.length();
                char c;
                try {
                    for (int i = 0; i < n; i++) {
                        c = text.charAt(i);
                        if (Character.isHighSurrogate(c)) {
                            int codePoint;
                            if (++i < n) {
                                codePoint = Character.toCodePoint(c, text.charAt(i));
                            } else {
                                codePoint = '\ufffd';
                            }
                            mapAndSend(codePoint);
                        } else {
                            mapAndSend(c);
                        }
                    }
                } catch (IOException e) {

                }
            }

            private void mapAndSend(int c) throws IOException {
                int result = mKeyListener.mapControlChar(c);
                if (result < TermKeyListener.KEYCODE_OFFSET) {
                    mTermSession.write(result);
                } else {
                    mKeyListener.handleKeyCode(result - TermKeyListener.KEYCODE_OFFSET, getKeypadApplicationMode());
                }
                clearSpecialKeyStatus();
            }

            public boolean beginBatchEdit() {
                setImeBuffer("");
                mCursor = 0;
                mComposingTextStart = 0;
                mComposingTextEnd = 0;
                return true;
            }

            public boolean clearMetaKeyStates(int arg0) {
                return false;
            }

            public boolean commitCompletion(CompletionInfo arg0) {
                return false;
            }

            public boolean endBatchEdit() {

                return true;
            }

            public boolean finishComposingText() {

                sendText(mImeBuffer);
                setImeBuffer("");
                mComposingTextStart = 0;
                mComposingTextEnd = 0;
                mCursor = 0;
                return true;
            }

            public int getCursorCapsMode(int reqModes) {

                int mode = 0;
                if ((reqModes & TextUtils.CAP_MODE_CHARACTERS) != 0) {
                    mode |= TextUtils.CAP_MODE_CHARACTERS;
                }
                return mode;
            }

            public ExtractedText getExtractedText(ExtractedTextRequest arg0, int arg1) {
                return null;
            }

            public CharSequence getTextAfterCursor(int n, int flags) {

                int len = Math.min(n, mImeBuffer.length() - mCursor);
                if (len <= 0 || mCursor < 0 || mCursor >= mImeBuffer.length()) {
                    return "";
                }
                return mImeBuffer.substring(mCursor, mCursor + len);
            }

            public CharSequence getTextBeforeCursor(int n, int flags) {

                int len = Math.min(n, mCursor);
                if (len <= 0 || mCursor < 0 || mCursor >= mImeBuffer.length()) {
                    return "";
                }
                return mImeBuffer.substring(mCursor - len, mCursor);
            }

            public boolean performContextMenuAction(int arg0) {

                return true;
            }

            public boolean performPrivateCommand(String arg0, Bundle arg1) {

                return true;
            }

            public boolean reportFullscreenMode(boolean arg0) {

                return true;
            }

            public boolean commitCorrection(CorrectionInfo correctionInfo) {

                return true;
            }

            public boolean commitText(CharSequence text, int newCursorPosition) {

                clearComposingText();
                sendText(text);
                setImeBuffer("");
                mCursor = 0;
                return true;
            }

            private void clearComposingText() {
                int len = mImeBuffer.length();
                if (mComposingTextStart > len || mComposingTextEnd > len) {
                    mComposingTextEnd = mComposingTextStart = 0;
                    return;
                }
                setImeBuffer(mImeBuffer.substring(0, mComposingTextStart) + mImeBuffer.substring(mComposingTextEnd));
                if (mCursor < mComposingTextStart) {

                } else if (mCursor < mComposingTextEnd) {
                    mCursor = mComposingTextStart;
                } else {
                    mCursor -= mComposingTextEnd - mComposingTextStart;
                }
                mComposingTextEnd = mComposingTextStart = 0;
            }

            public boolean deleteSurroundingText(int leftLength, int rightLength) {

                if (leftLength > 0) {
                    for (int i = 0; i < leftLength; i++) {
                        sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL));
                    }
                } else if ((leftLength == 0) && (rightLength == 0)) {
                    sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL));
                }
                return true;
            }

            public boolean performEditorAction(int actionCode) {

                if (actionCode == EditorInfo.IME_ACTION_UNSPECIFIED) {
                    sendText("\r");
                }
                return true;
            }

            public boolean sendKeyEvent(KeyEvent event) {

                dispatchKeyEvent(event);
                return true;
            }

            public boolean setComposingText(CharSequence text, int newCursorPosition) {

                int len = mImeBuffer.length();
                if (mComposingTextStart > len || mComposingTextEnd > len) {
                    return false;
                }
                setImeBuffer(mImeBuffer.substring(0, mComposingTextStart) + text + mImeBuffer.substring(mComposingTextEnd));
                mComposingTextEnd = mComposingTextStart + text.length();
                mCursor = newCursorPosition > 0 ? mComposingTextEnd + newCursorPosition - 1 : mComposingTextStart - newCursorPosition;
                return true;
            }

            public boolean setSelection(int start, int end) {

                int length = mImeBuffer.length();
                if (start == end && start > 0 && start < length) {
                    mSelectedTextStart = mSelectedTextEnd = 0;
                    mCursor = start;
                } else if (start < end && start > 0 && end < length) {
                    mSelectedTextStart = start;
                    mSelectedTextEnd = end;
                    mCursor = start;
                }
                return true;
            }

            public boolean setComposingRegion(int start, int end) {

                if (start < end && start > 0 && end < mImeBuffer.length()) {
                    clearComposingText();
                    mComposingTextStart = start;
                    mComposingTextEnd = end;
                }
                return true;
            }

            public CharSequence getSelectedText(int flags) {

                int len = mImeBuffer.length();
                if (mSelectedTextEnd >= len || mSelectedTextStart > mSelectedTextEnd) {
                    return "";
                }
                return mImeBuffer.substring(mSelectedTextStart, mSelectedTextEnd + 1);
            }

        };
    }

    private void setImeBuffer(String buffer) {
        if (!buffer.equals(mImeBuffer)) {
            invalidate();
        }
        mImeBuffer = buffer;
    }

    public boolean getKeypadApplicationMode() {
        return mEmulator.getKeypadApplicationMode();
    }

    public void setExtGestureListener(GestureDetector.OnGestureListener listener) {
        mExtGestureListener = listener;
    }

    @Override
    protected int computeVerticalScrollRange() {
        return mTranscriptScreen.getActiveRows();
    }

    @Override
    protected int computeVerticalScrollExtent() {
        return mRows;
    }

    @Override
    protected int computeVerticalScrollOffset() {
        return mTranscriptScreen.getActiveRows() + mTopRow - mRows;
    }

    private void initialize() {
        TermSession session = mTermSession;

        updateText();

        mTranscriptScreen = session.getTranscriptScreen();
        mEmulator = session.getEmulator();
        session.setUpdateCallback(mUpdateNotify);

        requestFocus();
    }

    public TermSession getTermSession() {
        return mTermSession;
    }

    public int getVisibleWidth() {
        return mVisibleWidth;
    }

    public int getVisibleHeight() {
        return mVisibleHeight;
    }

    public void page(int delta) {
        mTopRow = Math.min(0, Math.max(-(mTranscriptScreen.getActiveTranscriptRows()), mTopRow + mRows * delta));
        invalidate();
    }

    public void pageHorizontal(int deltaColumns) {
        mLeftColumn = Math.max(0, Math.min(mLeftColumn + deltaColumns, mColumns - mVisibleColumns));
        invalidate();
    }

    public void setTextSize(int fontSize) {
        mTextSize = (int) (fontSize * mDensity);
        updateText();
    }

    public void setCursorStyle(int blink) {
        if (blink != 0 && mCursorBlink == 0) {
            mHandler.postDelayed(mBlinkCursor, CURSOR_BLINK_PERIOD);
        } else if (blink == 0 && mCursorBlink != 0) {
            mHandler.removeCallbacks(mBlinkCursor);
        }
        mCursorBlink = blink;
    }

    public void setUseCookedIME(boolean useCookedIME) {
        mUseCookedIme = useCookedIME;
    }

    public boolean onSingleTapUp(MotionEvent e) {
        if (mExtGestureListener != null && mExtGestureListener.onSingleTapUp(e)) {
            return true;
        }
        requestFocus();
        return true;
    }

    public void onLongPress(MotionEvent e) {
        showContextMenu();
    }

    public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX, float distanceY) {
        if (mExtGestureListener != null && mExtGestureListener.onScroll(e1, e2, distanceX, distanceY)) {
            return true;
        }
        distanceY += mScrollRemainder;
        int deltaRows = (int) (distanceY / mCharacterHeight);
        mScrollRemainder = distanceY - deltaRows * mCharacterHeight;
        mTopRow = Math.min(0, Math.max(-(mTranscriptScreen.getActiveTranscriptRows()), mTopRow + deltaRows));
        invalidate();

        return true;
    }

    public void onSingleTapConfirmed(MotionEvent e) {
    }

    public boolean onJumpTapDown(MotionEvent e1, MotionEvent e2) {
        mTopRow = 0;
        invalidate();
        return true;
    }

    public boolean onJumpTapUp(MotionEvent e1, MotionEvent e2) {
        mTopRow = -mTranscriptScreen.getActiveTranscriptRows();
        invalidate();
        return true;
    }

    public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
        if (mExtGestureListener != null && mExtGestureListener.onFling(e1, e2, velocityX, velocityY)) {
            return true;
        }
        float SCALE = 0.25f;
        mScroller.fling(0, mTopRow, -(int) (velocityX * SCALE), -(int) (velocityY * SCALE), 0, 0, -mTranscriptScreen.getActiveTranscriptRows(), 0);
        mScrollRemainder = 0.0f;
        post(mFlingRunner);
        return true;
    }

    public void onShowPress(MotionEvent e) {
        if (mExtGestureListener != null) {
            mExtGestureListener.onShowPress(e);
        }
    }

    public boolean onDown(MotionEvent e) {
        if (mExtGestureListener != null && mExtGestureListener.onDown(e)) {
            return true;
        }
        mScrollRemainder = 0.0f;
        return true;
    }

    @Override
    public boolean onTouchEvent(MotionEvent ev) {
        if (mIsSelectingText) {
            return onTouchEventWhileSelectingText(ev);
        } else {
            return mGestureDetector.onTouchEvent(ev);
        }
    }

    private boolean onTouchEventWhileSelectingText(MotionEvent ev) {
        int action = ev.getAction();
        int cx = (int) (ev.getX() / mCharacterWidth);
        int cy = Math.max(0, (int) ((ev.getY() + SELECT_TEXT_OFFSET_Y * mScaledDensity) / mCharacterHeight) + mTopRow);
        switch (action) {
            case MotionEvent.ACTION_DOWN:
                mSelXAnchor = cx;
                mSelYAnchor = cy;
                mSelX1 = cx;
                mSelY1 = cy;
                mSelX2 = mSelX1;
                mSelY2 = mSelY1;
                break;
            case MotionEvent.ACTION_MOVE:
            case MotionEvent.ACTION_UP:
                int minx = Math.min(mSelXAnchor, cx);
                int maxx = Math.max(mSelXAnchor, cx);
                int miny = Math.min(mSelYAnchor, cy);
                int maxy = Math.max(mSelYAnchor, cy);
                mSelX1 = minx;
                mSelY1 = miny;
                mSelX2 = maxx;
                mSelY2 = maxy;
                if (action == MotionEvent.ACTION_UP) {
                    toggleSelectingText();
                }
                invalidate();
                break;
            default:
                toggleSelectingText();
                invalidate();
                break;
        }
        return true;
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {

        if (handleControlKey(keyCode, true)) {
            return true;
        } else if (handleFnKey(keyCode, true)) {
            return true;
        } else if (isSystemKey(keyCode, event)) {
            if (!isInterceptedSystemKey(keyCode)) {
                return super.onKeyDown(keyCode, event);
            }
        }
        try {
            int oldCombiningAccent = mKeyListener.getCombiningAccent();
            int oldCursorMode = mKeyListener.getCursorMode();
            mKeyListener.keyDown(keyCode, event, getKeypadApplicationMode(), TermKeyListener.isEventFromToggleDevice(event));
            if (mKeyListener.getCombiningAccent() != oldCombiningAccent || mKeyListener.getCursorMode() != oldCursorMode) {
                invalidate();
            }
        } catch (IOException e) {
        }
        return true;
    }

    private boolean isInterceptedSystemKey(int keyCode) {
        return keyCode == KeyEvent.KEYCODE_BACK && mBackKeySendsCharacter;
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {

        if (handleControlKey(keyCode, false)) {
            return true;
        } else if (handleFnKey(keyCode, false)) {
            return true;
        } else if (isSystemKey(keyCode, event)) {
            if (!isInterceptedSystemKey(keyCode)) {
                return super.onKeyUp(keyCode, event);
            }
        }

        mKeyListener.keyUp(keyCode, event);
        clearSpecialKeyStatus();
        return true;
    }

    @Override
    public boolean onKeyPreIme(int keyCode, KeyEvent event) {
        if (sTrapAltAndMeta) {
            boolean altEsc = mKeyListener.getAltSendsEsc();
            boolean altOn = (event.getMetaState() & KeyEvent.META_ALT_ON) != 0;
            boolean metaOn = (event.getMetaState() & KeyEvent.META_META_ON) != 0;
            boolean altPressed = (keyCode == KeyEvent.KEYCODE_ALT_LEFT) || (keyCode == KeyEvent.KEYCODE_ALT_RIGHT);
            boolean altActive = mKeyListener.isAltActive();
            if (altEsc && (altOn || altPressed || altActive || metaOn)) {
                if (event.getAction() == KeyEvent.ACTION_DOWN) {
                    return onKeyDown(keyCode, event);
                } else {
                    return onKeyUp(keyCode, event);
                }
            }
        }

        if (handleHardwareControlKey(keyCode, event)) {
            return true;
        }

        if (mKeyListener.isCtrlActive()) {
            if (event.getAction() == KeyEvent.ACTION_DOWN) {
                return onKeyDown(keyCode, event);
            } else {
                return onKeyUp(keyCode, event);
            }
        }

        return super.onKeyPreIme(keyCode, event);
    }

    ;

    private boolean handleControlKey(int keyCode, boolean down) {
        if (keyCode == mControlKeyCode) {

            mKeyListener.handleControlKey(down);
            invalidate();
            return true;
        }
        return false;
    }

    private boolean handleHardwareControlKey(int keyCode, KeyEvent event) {
        if (keyCode == TermKeyListener.KEYCODE_CTRL_LEFT || keyCode == TermKeyListener.KEYCODE_CTRL_RIGHT) {

            boolean down = event.getAction() == KeyEvent.ACTION_DOWN;
            mKeyListener.handleHardwareControlKey(down);
            invalidate();
            return true;
        }
        return false;
    }

    private boolean handleFnKey(int keyCode, boolean down) {
        if (keyCode == mFnKeyCode) {

            mKeyListener.handleFnKey(down);
            invalidate();
            return true;
        }
        return false;
    }

    private boolean isSystemKey(int keyCode, KeyEvent event) {
        return event.isSystem();
    }

    private void clearSpecialKeyStatus() {
        if (mIsControlKeySent) {
            mIsControlKeySent = false;
            mKeyListener.handleControlKey(false);
            invalidate();
        }
        if (mIsFnKeySent) {
            mIsFnKeySent = false;
            mKeyListener.handleFnKey(false);
            invalidate();
        }
    }

    private void updateText() {
        ColorScheme scheme = mColorScheme;

        mTextRenderer = new PaintRenderer(mTextSize, scheme);

        mForegroundPaint.setColor(scheme.getForeColor());
        mBackgroundPaint.setColor(scheme.getBackColor());
        mCharacterWidth = mTextRenderer.getCharacterWidth();
        mCharacterHeight = mTextRenderer.getCharacterHeight();

        updateSize(true);
    }

    @Override
    protected void onSizeChanged(int w, int h, int oldw, int oldh) {
        if (mTermSession == null) {
            mDeferInit = true;
            return;
        }

        if (!mKnownSize) {
            mKnownSize = true;
            initialize();
        } else {
            updateSize(false);
        }
    }

    private void updateSize(int w, int h) {
        mColumns = Math.max(1, (int) (((float) w) / mCharacterWidth));
        mVisibleColumns = (int) (((float) mVisibleWidth) / mCharacterWidth);

        mTopOfScreenMargin = mTextRenderer.getTopMargin();
        mRows = Math.max(1, (h - mTopOfScreenMargin) / mCharacterHeight);
        mTermSession.updateSize(mColumns, mRows);
        mTopRow = 0;
        mLeftColumn = 0;

        invalidate();
    }

    public void updateSize(boolean force) {
        if (mKnownSize) {
            int w = getWidth();
            int h = getHeight();
            if (force || w != mVisibleWidth || h != mVisibleHeight) {
                mVisibleWidth = w;
                mVisibleHeight = h;
                updateSize(mVisibleWidth, mVisibleHeight);
            }
        }
    }

    @Override
    protected void onDraw(Canvas canvas) {
        updateSize(false);

        if (mEmulator == null) {
            return;
        }

        int w = getWidth();
        int h = getHeight();

        boolean reverseVideo = mEmulator.getReverseVideo();
        mTextRenderer.setReverseVideo(reverseVideo);

        Paint backgroundPaint = reverseVideo ? mForegroundPaint : mBackgroundPaint;
        canvas.drawRect(0, 0, w, h, backgroundPaint);
        float x = -mLeftColumn * mCharacterWidth;
        float y = mCharacterHeight + mTopOfScreenMargin;
        int endLine = mTopRow + mRows;
        int cx = mEmulator.getCursorCol();
        int cy = mEmulator.getCursorRow();
        boolean cursorVisible = mCursorVisible && mEmulator.getShowCursor();
        String effectiveImeBuffer = mImeBuffer;
        int combiningAccent = mKeyListener.getCombiningAccent();
        if (combiningAccent != 0) {
            effectiveImeBuffer += String.valueOf((char) combiningAccent);
        }
        int cursorStyle = mKeyListener.getCursorMode();
        for (int i = mTopRow; i < endLine; i++) {
            int cursorX = -1;
            if (i == cy && cursorVisible) {
                cursorX = cx;
            }
            int selx1 = -1;
            int selx2 = -1;
            if (i >= mSelY1 && i <= mSelY2) {
                if (i == mSelY1) {
                    selx1 = mSelX1;
                }
                if (i == mSelY2) {
                    selx2 = mSelX2;
                } else {
                    selx2 = mColumns;
                }
            }
            mTranscriptScreen.drawText(i, canvas, x, y, mTextRenderer, cursorX, selx1, selx2, effectiveImeBuffer, cursorStyle);
            y += mCharacterHeight;
        }
    }

    private void ensureCursorVisible() {
        mTopRow = 0;
        if (mVisibleColumns > 0) {
            int cx = mEmulator.getCursorCol();
            int visibleCursorX = mEmulator.getCursorCol() - mLeftColumn;
            if (visibleCursorX < 0) {
                mLeftColumn = cx;
            } else if (visibleCursorX >= mVisibleColumns) {
                mLeftColumn = (cx - mVisibleColumns) + 1;
            }
        }
    }

    public void toggleSelectingText() {
        mIsSelectingText = !mIsSelectingText;
        setVerticalScrollBarEnabled(!mIsSelectingText);
        if (!mIsSelectingText) {
            mSelX1 = -1;
            mSelY1 = -1;
            mSelX2 = -1;
            mSelY2 = -1;
        }
    }

    public boolean getSelectingText() {
        return mIsSelectingText;
    }

    public String getSelectedText() {
        return mEmulator.getSelectedText(mSelX1, mSelY1, mSelX2, mSelY2);
    }

    public void sendControlKey() {
        mIsControlKeySent = true;
        mKeyListener.handleControlKey(true);
        invalidate();
    }

    public void sendFnKey() {
        mIsFnKeySent = true;
        mKeyListener.handleFnKey(true);
        invalidate();
    }

    public void setBackKeyCharacter(int keyCode) {
        mKeyListener.setBackKeyCharacter(keyCode);
        mBackKeySendsCharacter = (keyCode != 0);
    }

    public void setAltSendsEsc(boolean flag) {
        mKeyListener.setAltSendsEsc(flag);
    }

    public void setControlKeyCode(int keyCode) {
        mControlKeyCode = keyCode;
    }

    public void setFnKeyCode(int keyCode) {
        mFnKeyCode = keyCode;
    }

    public void setTermType(String termType) {
        mKeyListener.setTermType(termType);
    }
}
