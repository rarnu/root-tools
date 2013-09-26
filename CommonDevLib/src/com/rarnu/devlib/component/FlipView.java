package com.rarnu.devlib.component;

import android.content.Context;
import android.content.res.Configuration;
import android.content.res.TypedArray;
import android.database.DataSetObserver;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;
import android.opengl.GLSurfaceView;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewConfiguration;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.Adapter;
import android.widget.AdapterView;
import com.rarnu.devlib.R;
import com.rarnu.devlib.component.tools.FlipCards;
import com.rarnu.devlib.component.tools.FlipRenderer;

import java.util.LinkedList;

public class FlipView extends AdapterView<Adapter> {

    public static final int VERTICAL = 0;
    public static final int HORIZONTAL = 1;
    public static final int MSG_SURFACE_CREATED = 1;
    private static final int MAX_RELEASED_VIEW_SIZE = 1;
    private final LinkedList<View> bufferedViews = new LinkedList<View>();
    private final LinkedList<View> releasedViews = new LinkedList<View>();
    private final int sideBufferSize = 1;
    private Handler handler = new Handler(new Handler.Callback() {
        @Override
        public boolean handleMessage(Message msg) {
            if (msg.what == MSG_SURFACE_CREATED) {
                contentWidth = 0;
                contentHeight = 0;
                requestLayout();
                return true;
            }
            return false;
        }
    });
    private GLSurfaceView surfaceView;
    private FlipRenderer renderer;
    private FlipCards cards;
    private int contentWidth;
    private int contentHeight;
    private int flipOrientation;
    private volatile boolean inFlipAnimation = false;
    private Adapter adapter;
    private int adapterDataCount = 0;
    private DataSetObserver adapterDataObserver;
    private int bufferIndex = -1;
    private int adapterIndex = -1;
    private float touchSlop;
    private ViewFlipListener onViewFlipListener;
    private boolean overFlipEnabled = true;
    private boolean flipByTouchEnabled = true;
    private Bitmap.Config animationBitmapFormat = Bitmap.Config.ARGB_8888;

    public FlipView(Context context) {
        this(context, VERTICAL);
    }

    public FlipView(Context context, int flipOrientation) {
        super(context);
        init(context, flipOrientation);
    }

    public FlipView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);

        int orientation = VERTICAL;

        TypedArray a = context.getTheme().obtainStyledAttributes(attrs, R.styleable.FlipView, 0, 0);

        try {
            int value = a.getInteger(R.styleable.FlipView_orientation, VERTICAL);
            if (value == HORIZONTAL) {
                orientation = HORIZONTAL;
            }

            value = a.getInteger(R.styleable.FlipView_animationBitmapFormat, 0);
            if (value == 1) {
                setAnimationBitmapFormat(Bitmap.Config.ARGB_4444);
            } else if (value == 2) {
                setAnimationBitmapFormat(Bitmap.Config.RGB_565);
            } else {
                setAnimationBitmapFormat(Bitmap.Config.ARGB_8888);
            }
        } finally {
            a.recycle();
        }

        init(context, orientation);
    }

    public FlipView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    private void init(Context context, int orientation) {
        ViewConfiguration configuration = ViewConfiguration.get(getContext());
        touchSlop = configuration.getScaledTouchSlop();
        this.flipOrientation = orientation;
        setupSurfaceView(context);
    }

    public Bitmap.Config getAnimationBitmapFormat() {
        return animationBitmapFormat;
    }

    public void setAnimationBitmapFormat(Bitmap.Config animationBitmapFormat) {
        this.animationBitmapFormat = animationBitmapFormat;
    }

    public ViewFlipListener getOnViewFlipListener() {
        return onViewFlipListener;
    }

    public void setOnViewFlipListener(ViewFlipListener onViewFlipListener) {
        this.onViewFlipListener = onViewFlipListener;
    }

    public void onResume() {
        surfaceView.onResume();
    }

    public void onPause() {
        surfaceView.onPause();
    }

    public void refreshPage(View pageView) {
        if (cards.refreshPageView(pageView)) {
            requestLayout();
        }
    }

    public void refreshPage(int pageIndex) {
        if (cards.refreshPage(pageIndex)) {
            requestLayout();
        }
    }

    public void refreshAllPages() {
        cards.refreshAllPages();
        requestLayout();
    }

    public boolean isOverFlipEnabled() {
        return overFlipEnabled;
    }

    public void setOverFlipEnabled(boolean overFlipEnabled) {
        this.overFlipEnabled = overFlipEnabled;
    }

    public boolean isFlipByTouchEnabled() {
        return flipByTouchEnabled;
    }

    public void setFlipByTouchEnabled(boolean flipByTouchEnabled) {
        this.flipByTouchEnabled = flipByTouchEnabled;
    }

    @Override
    public boolean onInterceptTouchEvent(MotionEvent event) {
        if (flipByTouchEnabled) {
            return cards.handleTouchEvent(event, false);
        } else {
            return false;
        }
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (flipByTouchEnabled) {
            return cards.handleTouchEvent(event, true);
        } else {
            return false;
        }
    }

    @Override
    protected void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    @Override
    public Adapter getAdapter() {
        return adapter;
    }

    @Override
    public void setAdapter(Adapter adapter) {
        setAdapter(adapter, 0);
    }

    public void setAdapter(Adapter adapter, int initialPosition) {
        if (this.adapter != null) {
            this.adapter.unregisterDataSetObserver(adapterDataObserver);
        }

        this.adapter = adapter;
        adapterDataCount = adapter.getCount();

        adapterDataObserver = new MyDataSetObserver();
        this.adapter.registerDataSetObserver(adapterDataObserver);
        if (adapterDataCount > 0) {
            setSelection(initialPosition);
        }
    }

    @Override
    public View getSelectedView() {
        return (bufferIndex < bufferedViews.size() && bufferIndex >= 0) ? bufferedViews.get(bufferIndex) : null;
    }

    @Override
    public void setSelection(int position) {
        if (adapter == null) {
            return;
        }
        releaseViews();

        View selectedView = viewFromAdapter(position, true);
        bufferedViews.add(selectedView);

        for (int i = 1; i <= sideBufferSize; i++) {
            int previous = position - i;
            int next = position + i;

            if (previous >= 0) {
                bufferedViews.addFirst(viewFromAdapter(previous, false));
            }
            if (next < adapterDataCount) {
                bufferedViews.addLast(viewFromAdapter(next, true));
            }
        }

        bufferIndex = bufferedViews.indexOf(selectedView);
        adapterIndex = position;

        requestLayout();
        updateVisibleView(inFlipAnimation ? -1 : bufferIndex);

        cards.resetSelection(position, adapterDataCount);
    }

    @Override
    public int getSelectedItemPosition() {
        return adapterIndex;
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {

        for (View child : bufferedViews) {
            child.layout(0, 0, r - l, b - t);
        }

        if (changed || contentWidth == 0) {
            int w = r - l;
            int h = b - t;
            surfaceView.layout(0, 0, w, h);

            if (contentWidth != w || contentHeight != h) {
                contentWidth = w;
                contentHeight = h;
            }
        }

        if (bufferedViews.size() >= 1) {
            View frontView = bufferedViews.get(bufferIndex);
            View backView = null;
            if (bufferIndex < bufferedViews.size() - 1) {
                backView = bufferedViews.get(bufferIndex + 1);
            }
            renderer.updateTexture(adapterIndex, frontView, backView == null ? -1 : adapterIndex + 1, backView);
        }
    }

    @Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);

        for (View child : bufferedViews) {
            child.measure(widthMeasureSpec, heightMeasureSpec);
        }

        surfaceView.measure(widthMeasureSpec, heightMeasureSpec);
    }

    public float getTouchSlop() {
        return touchSlop;
    }

    public GLSurfaceView getSurfaceView() {
        return surfaceView;
    }

    FlipRenderer getRenderer() {
        return renderer;
    }

    public int getContentWidth() {
        return contentWidth;
    }

    public int getContentHeight() {
        return contentHeight;
    }

    public void sendMessage(int message) {
        handler.sendMessage(Message.obtain(handler, message));
    }

    private void setupSurfaceView(Context context) {
        surfaceView = new GLSurfaceView(getContext());

        cards = new FlipCards(this, flipOrientation == VERTICAL);
        renderer = new FlipRenderer(this, cards);

        surfaceView.setEGLConfigChooser(8, 8, 8, 8, 16, 0);
        surfaceView.setZOrderOnTop(true);
        surfaceView.setRenderer(renderer);
        surfaceView.getHolder().setFormat(PixelFormat.TRANSLUCENT);
        surfaceView.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);

        addViewInLayout(surfaceView, -1, new AbsListView.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT), false);
    }

    private void releaseViews() {
        for (View view : bufferedViews) {
            releaseView(view);
        }
        bufferedViews.clear();
        bufferIndex = -1;
        adapterIndex = -1;
    }

    private void releaseView(View view) {
        detachViewFromParent(view);
        addReleasedView(view);
    }

    private void addReleasedView(View view) {
        if (releasedViews.size() < MAX_RELEASED_VIEW_SIZE) {
            releasedViews.add(view);
        }
    }

    private View viewFromAdapter(int position, boolean addToTop) {

        View releasedView = releasedViews.isEmpty() ? null : releasedViews.removeFirst();

        View view = adapter.getView(position, releasedView, this);
        if (releasedView != null && view != releasedView) {
            addReleasedView(releasedView);
        }

        setupAdapterView(view, addToTop, view == releasedView);
        return view;
    }

    private void setupAdapterView(View view, boolean addToTop, boolean isReusedView) {
        LayoutParams params = view.getLayoutParams();
        if (params == null) {
            params = new AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT, 0);
        }

        if (isReusedView) {
            attachViewToParent(view, addToTop ? 0 : 1, params);
        } else {
            addViewInLayout(view, addToTop ? 0 : 1, params, true);
        }
    }

    private void updateVisibleView(int index) {
        for (int i = 0; i < bufferedViews.size(); i++) {
            bufferedViews.get(i).setVisibility(index == i ? VISIBLE : GONE);
        }
    }

    public void postFlippedToView(final int indexInAdapter) {
        handler.post(new Runnable() {
            @Override
            public void run() {
                flippedToView(indexInAdapter, true);
            }
        });
    }

    public void flippedToView(final int indexInAdapter, boolean isPost) {

        if (indexInAdapter >= 0 && indexInAdapter < adapterDataCount) {

            if (indexInAdapter == adapterIndex + 1) {
                if (adapterIndex < adapterDataCount - 1) {
                    adapterIndex++;
                    View old = bufferedViews.get(bufferIndex);
                    if (bufferIndex + 1 > sideBufferSize) {
                        releaseView(bufferedViews.removeFirst());
                    }
                    if (adapterIndex + sideBufferSize < adapterDataCount) {
                        bufferedViews.addLast(viewFromAdapter(adapterIndex + sideBufferSize, true));
                    }
                    bufferIndex = bufferedViews.indexOf(old) + 1;
                    requestLayout();
                    updateVisibleView(bufferIndex);
                }
            } else if (indexInAdapter == adapterIndex - 1) {
                if (adapterIndex > 0) {
                    adapterIndex--;
                    View old = bufferedViews.get(bufferIndex);
                    if (bufferIndex - 1 + sideBufferSize < bufferedViews.size() - 1) {
                        releaseView(bufferedViews.removeLast());
                    }
                    if (adapterIndex - sideBufferSize >= 0) {
                        bufferedViews.addFirst(viewFromAdapter(adapterIndex - sideBufferSize, false));
                    }
                    bufferIndex = bufferedViews.indexOf(old) - 1;
                    requestLayout();
                    updateVisibleView(bufferIndex);
                }
            }
        }
    }

    public void showFlipAnimation() {
        if (!inFlipAnimation) {
            inFlipAnimation = true;

            cards.setVisible(true);
            cards.setFirstDrawFinished(false);
            surfaceView.requestRender();
        }
    }

    public void postHideFlipAnimation() {
        if (inFlipAnimation) {
            handler.post(new Runnable() {
                @Override
                public void run() {
                    hideFlipAnimation();
                }
            });
        }
    }

    private void hideFlipAnimation() {
        if (inFlipAnimation) {
            inFlipAnimation = false;

            updateVisibleView(bufferIndex);

            if (onViewFlipListener != null) {
                onViewFlipListener.onViewFlipped(bufferedViews.get(bufferIndex), adapterIndex);
            }

            handler.post(new Runnable() {
                public void run() {
                    if (!inFlipAnimation) {
                        cards.setVisible(false);
                        surfaceView.requestRender();
                    }
                }
            });
        }
    }

    private void onDataChanged() {
        adapterDataCount = adapter.getCount();
        int activeIndex;
        if (adapterIndex < 0) {
            activeIndex = 0;
        } else {
            activeIndex = Math.min(adapterIndex, adapterDataCount - 1);
        }

        releaseViews();
        setSelection(activeIndex);
    }

    public static interface ViewFlipListener {

        void onViewFlipped(View view, int position);
    }

    private class MyDataSetObserver extends DataSetObserver {

        @Override
        public void onChanged() {
            onDataChanged();
        }

        @Override
        public void onInvalidated() {
            onDataChanged();
        }
    }
}