package com.rarnu.findaround.comp;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.findaround.R;

public class ButtonEx extends RelativeLayout {

	private boolean fixed;
	private Button btn;
	private Button btnDelete;
	private TextView tv;
	private ImageView img;
	private boolean edit;

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

	public Button getButton() {
		return btn;
	}

	public void setIcon(int resid) {
		img.setBackgroundResource(resid);
	}

	public void setIcon(Drawable d) {
		img.setBackgroundDrawable(d);
	}

	public void setIcon(Bitmap bmp) {
		img.setImageBitmap(bmp);
	}

	public Button getDeleteButton() {
		return btnDelete;
	}

	private void init() {
		addView(inflate(getContext(), R.layout.welcome_button, null));
		btn = (Button) findViewById(R.id.btn);
		tv = (TextView) findViewById(R.id.tv);
		img = (ImageView) findViewById(R.id.img);
		btnDelete = (Button) findViewById(R.id.btnDelete);
		setClickable(true);
	}

	public void setFixed(boolean fixed) {
		this.fixed = fixed;
		btn.setBackgroundResource(fixed ? R.drawable.fix_button_style
				: R.drawable.button_style);
	}

	public boolean isFixed() {
		return fixed;
	}

	public void setText(String text) {
		tv.setText(text);
		btn.setTag("click|" + text);
		btnDelete.setTag("delete|" + text);
	}

	public String getText() {
		return tv.getText().toString();
	}

	public void setMode(boolean edit) {
		this.edit = edit;

		if (getVisibility() == View.VISIBLE) {
			btnDelete.setVisibility(edit ? View.VISIBLE : View.INVISIBLE);
			btnDelete.setBackgroundResource(fixed ? R.drawable.locked
					: R.drawable.cancel);
			if (edit) {
				Animation animation = AnimationUtils.loadAnimation(
						getContext(), R.anim.anim_shake);
				btn.setAnimation(animation);
				img.setAnimation(animation);

				animation.start();
			} else {
				btn.setAnimation(null);
				img.setAnimation(null);
			}
		}
	}

	public boolean getMode() {
		return edit;
	}

}
