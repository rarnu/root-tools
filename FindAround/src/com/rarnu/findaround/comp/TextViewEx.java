package com.rarnu.findaround.comp;

import com.rarnu.findaround.GlobalInstance;
import com.rarnu.findaround.R;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.Gallery;
import android.widget.TextView;

public class TextViewEx extends TextView implements OnTouchListener {

	private Gallery g;

	public TextViewEx(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public TextViewEx(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public TextViewEx(Context context) {
		super(context);
		init();
	}

	private void init() {
		setOnTouchListener(this);
	}

	public void setGallery(Gallery g) {
		this.g = g;
	}

	@Override
	public boolean onTouch(View v, MotionEvent event) {

		if (GlobalInstance.onTouchMutax) {
			return true;
		}

		GlobalInstance.onTouchMutax = true;
		if (event.getAction() == MotionEvent.ACTION_DOWN) {
			
			setBackgroundResource(R.drawable.button_focus);
		} else if (event.getAction() == MotionEvent.ACTION_UP) {
			setBackgroundResource(R.drawable.button);
			
		}
		if (g != null) {
			g.onTouchEvent(event);
		}
		GlobalInstance.onTouchMutax = false;
		return false;
	}
}
