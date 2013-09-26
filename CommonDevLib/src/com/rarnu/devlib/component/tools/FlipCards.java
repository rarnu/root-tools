package com.rarnu.devlib.component.tools;

import android.view.MotionEvent;
import android.view.View;
import com.rarnu.devlib.component.FlipView;

import javax.microedition.khronos.opengles.GL10;

public class FlipCards {

    private static final float ACCELERATION = 0.65f;
    private static final float MOVEMENT_RATE = 1.5f;
    private static final int MAX_TIP_ANGLE = 60;
    private static final int MAX_TOUCH_MOVE_ANGLE = 15;
    private static final float MIN_MOVEMENT = 4f;
    private static final int STATE_INIT = 0;
    private static final int STATE_TOUCH = 1;
    private static final int STATE_AUTO_ROTATE = 2;
    private ViewDualCards frontCards;
    private ViewDualCards backCards;
    private float accumulatedAngle = 0f;
    private boolean forward = true;
    private int animatedFrame = 0;
    private int state = STATE_INIT;
    private boolean orientationVertical = true;
    private float lastPosition = -1;
    private FlipView controller;
    private volatile boolean visible = false;
    private volatile boolean firstDrawFinished = false;
    private int maxIndex = 0;
    private int lastPageIndex;

    public FlipCards(FlipView controller, boolean orientationVertical) {
        this.controller = controller;

        frontCards = new ViewDualCards(orientationVertical);
        backCards = new ViewDualCards(orientationVertical);
        this.orientationVertical = orientationVertical;
    }

    public boolean isVisible() {
        return visible;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    public boolean isFirstDrawFinished() {
        return firstDrawFinished;
    }

    public void setFirstDrawFinished(boolean firstDrawFinished) {
        this.firstDrawFinished = firstDrawFinished;
    }

    public boolean refreshPageView(View view) {
        boolean match = false;
        if (frontCards.getView() == view) {
            frontCards.resetWithIndex(frontCards.getIndex());
            match = true;
        }
        if (backCards.getView() == view) {
            backCards.resetWithIndex(backCards.getIndex());
            match = true;
        }

        return match;
    }

    public boolean refreshPage(int pageIndex) {
        boolean match = false;
        if (frontCards.getIndex() == pageIndex) {
            frontCards.resetWithIndex(pageIndex);
            match = true;
        }
        if (backCards.getIndex() == pageIndex) {
            backCards.resetWithIndex(pageIndex);
            match = true;
        }

        return match;
    }

    public void refreshAllPages() {
        frontCards.resetWithIndex(frontCards.getIndex());
        backCards.resetWithIndex(backCards.getIndex());
    }

    public void reloadTexture(int frontIndex, View frontView, int backIndex, View backView) {
        synchronized (this) {
            frontCards.loadView(frontIndex, frontView, controller.getAnimationBitmapFormat());
            backCards.loadView(backIndex, backView, controller.getAnimationBitmapFormat());

        }
    }

    public synchronized void resetSelection(int selection, int maxIndex) {
        FlipUI.assertInMainThread();
        this.maxIndex = maxIndex;
        setVisible(false);
        setState(STATE_INIT);
        accumulatedAngle = selection * 180;
        frontCards.resetWithIndex(selection);
        backCards.resetWithIndex(selection + 1 < maxIndex ? selection + 1 : -1);
        controller.postHideFlipAnimation();
    }

    public synchronized void draw(FlipRenderer renderer, GL10 gl) {
        frontCards.buildTexture(renderer, gl);
        backCards.buildTexture(renderer, gl);

        if (!Texture.isValidTexture(frontCards.getTexture()) && !Texture.isValidTexture(backCards.getTexture())) {
            return;
        }

        if (!visible) {
            return;
        }

        switch (state) {
            case STATE_INIT:
            case STATE_TOUCH:
                break;
            case STATE_AUTO_ROTATE: {
                animatedFrame++;
                float delta = (forward ? ACCELERATION : -ACCELERATION) * animatedFrame % 180;

                float oldAngle = accumulatedAngle;

                accumulatedAngle += delta;

                if (oldAngle < 0) {
                    if (accumulatedAngle >= 0) {
                        accumulatedAngle = 0;
                        setState(STATE_INIT);
                    }
                } else {
                    if (frontCards.getIndex() == maxIndex - 1 && oldAngle > frontCards.getIndex() * 180) {
                        if (accumulatedAngle <= frontCards.getIndex() * 180) {
                            setState(STATE_INIT);
                            accumulatedAngle = frontCards.getIndex() * 180;
                        }
                    } else {
                        if (forward) {

                            if (accumulatedAngle >= backCards.getIndex() * 180) {
                                accumulatedAngle = backCards.getIndex() * 180;
                                setState(STATE_INIT);
                                controller.postFlippedToView(backCards.getIndex());

                                swapCards();
                                backCards.resetWithIndex(frontCards.getIndex() + 1);
                            }
                        } else {
                            if (accumulatedAngle <= frontCards.getIndex() * 180) {
                                accumulatedAngle = frontCards.getIndex() * 180;
                                setState(STATE_INIT);
                            }
                        }
                    }
                }

                if (state == STATE_INIT) {
                    controller.postHideFlipAnimation();
                } else {
                    controller.getSurfaceView().requestRender();
                }
            }
            break;
            default:
                break;
        }

        float angle = getDisplayAngle();
        if (angle < 0) {
            frontCards.getTopCard().setAxis(Card.AXIS_BOTTOM);
            frontCards.getTopCard().setAngle(-angle);
            frontCards.getTopCard().draw(gl);

            frontCards.getBottomCard().setAngle(0);
            frontCards.getBottomCard().draw(gl);

        } else {
            if (angle < 90) {
                frontCards.getTopCard().setAngle(0);
                frontCards.getTopCard().draw(gl);

                backCards.getBottomCard().setAngle(0);
                backCards.getBottomCard().draw(gl);

                frontCards.getBottomCard().setAxis(Card.AXIS_TOP);
                frontCards.getBottomCard().setAngle(angle);
                frontCards.getBottomCard().draw(gl);
            } else {
                frontCards.getTopCard().setAngle(0);
                frontCards.getTopCard().draw(gl);

                backCards.getTopCard().setAxis(Card.AXIS_BOTTOM);
                backCards.getTopCard().setAngle(180 - angle);
                backCards.getTopCard().draw(gl);

                backCards.getBottomCard().setAngle(0);
                backCards.getBottomCard().draw(gl);
            }
        }

        if ((frontCards.getView() == null || Texture.isValidTexture(frontCards.getTexture())) && (backCards.getView() == null || Texture.isValidTexture(backCards.getTexture())))
            firstDrawFinished = true;
    }

    public void invalidateTexture() {
        frontCards.abandonTexture();
        backCards.abandonTexture();
    }

    public synchronized boolean handleTouchEvent(MotionEvent event, boolean isOnTouchEvent) {
        switch (event.getAction()) {
            case MotionEvent.ACTION_DOWN:
                lastPageIndex = getPageIndexFromAngle(accumulatedAngle);
                lastPosition = orientationVertical ? event.getY() : event.getX();
                return isOnTouchEvent;
            case MotionEvent.ACTION_MOVE:
                float delta = orientationVertical ? (lastPosition - event.getY()) : (lastPosition - event.getX());

                if (Math.abs(delta) > controller.getTouchSlop()) {
                    setState(STATE_TOUCH);
                    forward = delta > 0;
                }
                if (state == STATE_TOUCH) {
                    if (Math.abs(delta) > MIN_MOVEMENT) {
                        forward = delta > 0;
                    }

                    controller.showFlipAnimation();

                    float angleDelta;
                    if (orientationVertical) {
                        angleDelta = 180 * delta / controller.getContentHeight() * MOVEMENT_RATE;
                    } else {
                        angleDelta = 180 * delta / controller.getContentWidth() * MOVEMENT_RATE;
                    }

                    if (Math.abs(angleDelta) > MAX_TOUCH_MOVE_ANGLE) {
                        angleDelta = Math.signum(angleDelta) * MAX_TOUCH_MOVE_ANGLE;
                    }

                    if (Math.abs(getPageIndexFromAngle(accumulatedAngle + angleDelta) - lastPageIndex) <= 1) {
                        accumulatedAngle += angleDelta;
                    }

                    if (frontCards.getIndex() == maxIndex - 1) {
                        if (accumulatedAngle > frontCards.getIndex() * 180) {
                            accumulatedAngle = Math.min(accumulatedAngle, controller.isOverFlipEnabled() ? (frontCards.getIndex() * 180 + MAX_TIP_ANGLE) : (frontCards.getIndex() * 180));
                        }
                    }

                    if (accumulatedAngle < 0) {
                        accumulatedAngle = Math.max(accumulatedAngle, controller.isOverFlipEnabled() ? -MAX_TIP_ANGLE : 0);
                    }

                    int anglePageIndex = getPageIndexFromAngle(accumulatedAngle);

                    if (accumulatedAngle >= 0) {
                        if (anglePageIndex != frontCards.getIndex()) {
                            if (anglePageIndex == frontCards.getIndex() - 1) {
                                swapCards();
                                frontCards.resetWithIndex(backCards.getIndex() - 1);
                                controller.flippedToView(anglePageIndex, false);
                            } else if (anglePageIndex == frontCards.getIndex() + 1) {
                                swapCards();
                                backCards.resetWithIndex(frontCards.getIndex() + 1);
                                controller.flippedToView(anglePageIndex, false);
                            } else {
                                throw new RuntimeException();
                            }
                        }
                    }

                    lastPosition = orientationVertical ? event.getY() : event.getX();

                    controller.getSurfaceView().requestRender();
                    return true;
                }

                return isOnTouchEvent;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_CANCEL:
                if (state == STATE_TOUCH) {
                    if (accumulatedAngle < 0) {
                        forward = true;
                    } else if (accumulatedAngle >= frontCards.getIndex() * 180 && frontCards.getIndex() == maxIndex - 1) {
                        forward = false;
                    }

                    setState(STATE_AUTO_ROTATE);
                    controller.getSurfaceView().requestRender();
                }
                return isOnTouchEvent;
        }

        return false;
    }

    private void swapCards() {
        ViewDualCards tmp = frontCards;
        frontCards = backCards;
        backCards = tmp;
    }

    private void setState(int state) {
        if (this.state != state) {
            this.state = state;
            animatedFrame = 0;
        }
    }

    private int getPageIndexFromAngle(float angle) {
        return ((int) angle) / 180;
    }

    private float getDisplayAngle() {
        return accumulatedAngle % 180;
    }
}