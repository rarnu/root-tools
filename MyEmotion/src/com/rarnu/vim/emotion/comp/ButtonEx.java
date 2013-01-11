package com.rarnu.vim.emotion.comp;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.vim.emotion.R;

public class ButtonEx extends RelativeLayout {

	private TextView btn;
	private TextView tv;
	private ImageView img;
	
	private Object data;

	public ButtonEx(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public ButtonEx(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public ButtonEx(Context context) {
		super(context);
		init();
	}

	public TextView getButton() {
		return btn;
	}

	public void setIcon(Drawable drawable) {
		img.setImageDrawable(drawable);
	}

	private void init() {
		addView(inflate(getContext(), R.layout.face_button, null));
		btn = (TextView) findViewById(R.id.btn);
		tv = (TextView) findViewById(R.id.tv);
		img = (ImageView) findViewById(R.id.img);
		
		setClickable(true);
	}

	public void setText(String text) {
		tv.setText(text);
	}
	
	public void setExtraData(Object data) {
		this.data = data;
	}
	
	public Object getExtraData() {
		return data;
	}

	public String getText() {
		return tv.getText().toString();
	}

}
