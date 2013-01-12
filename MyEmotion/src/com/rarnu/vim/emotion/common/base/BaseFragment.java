package com.rarnu.vim.emotion.common.base;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;

import com.rarnu.vim.emotion.EmotionInterface;

public abstract class BaseFragment extends Fragment implements OnTouchListener {

	protected View innerView;
	
	public abstract int getFragmentLayout();
	public abstract void initComponents();
	public abstract void init();
	
	float X1 = 0F, Y1 = 0F, X2 = 0F, Y2 = 0F;
	float NX1 = 0F, NY1 = 0F, NX2 = 0F, NY2 = 0F;
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		innerView = inflater.inflate(getFragmentLayout(), container, false);
		initComponents();
		return innerView;
	}
	
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		init();
	}
	
	@Override
	public boolean onTouch(View v, MotionEvent event) {
		if (event.getPointerCount() < 2) {
			((EmotionInterface) getActivity()).setLeftRightScrollable(true);
			return false;
		}

		boolean isDown = event.getAction() == (MotionEvent.ACTION_POINTER_DOWN | 0x0100);
		boolean isUp = (event.getAction() == (MotionEvent.ACTION_POINTER_UP | 0x0100))
				|| (event.getAction() == MotionEvent.ACTION_CANCEL)
				|| (event.getAction() == (MotionEvent.ACTION_POINTER_UP | 0x0000));

		if (isDown) {
			((EmotionInterface) getActivity()).setLeftRightScrollable(false);

			X1 = event.getX(0);
			Y1 = event.getY(0);
			X2 = event.getX(1);
			Y2 = event.getY(1);
		} else if (isUp) {
			((EmotionInterface) getActivity()).setLeftRightScrollable(true);
			NX1 = event.getX(0);
			NY1 = event.getY(0);
			NX2 = event.getX(1);
			NY2 = event.getY(1);
			checkDistance();
		}

		return false;
	}

	private void checkDistance() {
		double dist = Math.sqrt(Math.pow(X1 - X2, 2) + Math.pow(Y1 - Y2, 2));
		double newDist = Math.sqrt(Math.pow(NX1 - NX2, 2)
				+ Math.pow(NY1 - NY2, 2));
		if ((newDist < dist) && (Math.abs(dist - newDist) > 150)) {
			doShrink();
		}
		if ((newDist > dist) && (Math.abs(newDist - dist) > 150)) {
			doExpand();
		}
	}
	
	public abstract void doShrink();
	public abstract void doExpand();
	
}
