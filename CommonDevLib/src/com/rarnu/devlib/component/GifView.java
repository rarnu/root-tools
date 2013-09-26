package com.rarnu.devlib.component;

import android.content.Context;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.os.Handler;
import android.os.Message;
import android.os.Parcelable;
import android.os.SystemClock;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import com.rarnu.devlib.component.tools.GifAction;
import com.rarnu.devlib.component.tools.GifDecoder;
import com.rarnu.devlib.component.tools.GifFrame;

import java.io.InputStream;

public class GifView extends ImageView implements GifAction {

    private GifDecoder gifDecoder = null;
    private Bitmap currentImage = null;
    private boolean isRun = true;
    private boolean pause = false;
    private DrawThread drawThread = null;
    @SuppressWarnings("unused")
    private Context context = null;
    @SuppressWarnings("unused")
    private boolean cacheImage = false;
    private View backView = null;
    private GifImageType animationType = GifImageType.SYNC_DECODER;
    private Handler redrawHandler = new Handler() {
        public void handleMessage(Message msg) {
            try {
                if (backView != null) {
                    backView.setBackgroundDrawable(new BitmapDrawable(currentImage));
                } else {
                    drawImage();
                }
            } catch (Exception ex) {

            }
        }
    };

    public GifView(Context context) {
        super(context);
        this.context = context;
        setScaleType(ImageView.ScaleType.FIT_XY);
    }

    public GifView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);

    }

    public GifView(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        this.context = context;

        setScaleType(ImageView.ScaleType.FIT_XY);
    }

    private void setGifDecoderImage(byte[] gif) {

        if (gifDecoder == null) {
            gifDecoder = new GifDecoder(this);
        }
        gifDecoder.setGifImage(gif);
        gifDecoder.start();
    }

    private void setGifDecoderImage(InputStream is) {

        if (gifDecoder == null) {
            gifDecoder = new GifDecoder(this);
        }
        gifDecoder.setGifImage(is);
        gifDecoder.start();

    }

    public void setAsBackground(View v) {
        backView = v;
    }

    protected Parcelable onSaveInstanceState() {
        super.onSaveInstanceState();
        if (gifDecoder != null)
            gifDecoder.free();

        return null;
    }

    public void setGifImage(byte[] gif) {
        setGifDecoderImage(gif);
    }

    public void setGifImage(InputStream is) {
        setGifDecoderImage(is);
    }

    public void setGifImage(int resId) {
        Resources r = getResources();
        InputStream is = r.openRawResource(resId);
        setGifDecoderImage(is);
    }

    public void destroy() {
        if (gifDecoder != null)
            gifDecoder.free();
    }

    public void showCover() {
        if (gifDecoder == null) {
            return;
        }
        pause = true;
        currentImage = gifDecoder.getImage();
        invalidate();
    }

    public void showAnimation() {
        if (pause) {
            pause = false;
        }
    }

    public void setGifImageType(GifImageType type) {
        if (gifDecoder == null)
            animationType = type;
    }

    public void parseOk(boolean parseStatus, int frameIndex) {
        if (parseStatus) {
            if (gifDecoder != null) {
                switch (animationType) {
                    case WAIT_FINISH:
                        if (frameIndex == -1) {
                            if (gifDecoder.getFrameCount() > 1) { // 当帧数大于1时，启动动画线程
                                DrawThread dt = new DrawThread();
                                dt.start();
                            } else {
                                reDraw();
                            }
                        }
                        break;
                    case COVER:
                        if (frameIndex == 1) {
                            currentImage = gifDecoder.getImage();
                            reDraw();
                        } else if (frameIndex == -1) {
                            if (gifDecoder.getFrameCount() > 1) {
                                if (drawThread == null) {
                                    drawThread = new DrawThread();
                                    drawThread.start();
                                }
                            } else {
                                reDraw();
                            }
                        }
                        break;
                    case SYNC_DECODER:
                        if (frameIndex == 1) {
                            currentImage = gifDecoder.getImage();
                            reDraw();
                        } else if (frameIndex == -1) {
                            reDraw();
                        } else {
                            if (drawThread == null) {
                                drawThread = new DrawThread();
                                drawThread.start();
                            }
                        }
                        break;
                }

            }

        }
    }

    private void reDraw() {
        if (redrawHandler != null) {
            Message msg = redrawHandler.obtainMessage();
            redrawHandler.sendMessage(msg);
        }

    }

    private void drawImage() {
        setImageBitmap(currentImage);
        invalidate();
    }

    public enum GifImageType {
        WAIT_FINISH(0), SYNC_DECODER(1), COVER(2);
        final int nativeInt;

        GifImageType(int i) {
            nativeInt = i;
        }
    }

    private class DrawThread extends Thread {
        public void run() {
            if (gifDecoder == null) {
                return;
            }
            while (isRun) {
                if (gifDecoder.getFrameCount() == 1) {
                    GifFrame f = gifDecoder.next();
                    currentImage = f.image;
                    gifDecoder.free();
                    reDraw();

                    break;
                }
                if (pause == false) {
                    GifFrame frame = gifDecoder.next();

                    if (frame == null) {
                        SystemClock.sleep(50);
                        continue;
                    }
                    if (frame.image != null) {
                        currentImage = frame.image;
                    } else if (frame.imageName != null) {
                        currentImage = BitmapFactory
                                .decodeFile(frame.imageName);
                    }
                    long sp = frame.delay;
                    if (redrawHandler != null) {
                        reDraw();
                        SystemClock.sleep(sp);
                    } else {
                        break;
                    }
                } else {
                    SystemClock.sleep(50);
                }
            }
        }
    }

}
