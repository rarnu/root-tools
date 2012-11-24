package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.loving.utils.UIUtils;

public class BottomBar extends RelativeLayout {

	public static final int BUTTON_1 = 1;
	public static final int BUTTON_2 = 2;
	public static final int BUTTON_3 = 3;
	public static final int BUTTON_4 = 4;

	BottomButton btn1, btn2, btn3, btn4;
	OnClickListener listener = null;

	public BottomBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public BottomBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public BottomBar(Context context) {
		super(context);
		init();
	}

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, UIUtils.dipToPx(56)));
		View v = inflate(getContext(), R.layout.comp_bottom_bar, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, UIUtils.dipToPx(56)));
		addView(v);

		btn1 = (BottomButton) findViewById(R.id.btn1);
		btn2 = (BottomButton) findViewById(R.id.btn2);
		btn3 = (BottomButton) findViewById(R.id.btn3);
		btn4 = (BottomButton) findViewById(R.id.btn4);

		resize();
	}

	private void resize() {
		int itemWidth = UIUtils.getWidth() / 4;
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btn1
				.getLayoutParams();
		rlp.width = itemWidth;
		btn1.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) btn2.getLayoutParams();
		rlp.width = itemWidth;
		btn2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) btn3.getLayoutParams();
		rlp.width = itemWidth;
		btn3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) btn4.getLayoutParams();
		rlp.width = itemWidth;
		btn4.setLayoutParams(rlp);
	}

	public void setText(int index, int res) {
		switch (index) {
		case BUTTON_1:
			btn1.setText(res);
			break;
		case BUTTON_2:
			btn2.setText(res);
			break;
		case BUTTON_3:
			btn3.setText(res);
			break;
		case BUTTON_4:
			btn4.setText(res);
			break;
		}
	}

	public void setText(int index, CharSequence text) {
		switch (index) {
		case BUTTON_1:
			btn1.setText(text);
			break;
		case BUTTON_2:
			btn2.setText(text);
			break;
		case BUTTON_3:
			btn3.setText(text);
			break;
		case BUTTON_4:
			btn4.setText(text);
			break;
		}
	}
	
	public void setOnButtonClick(OnClickListener listener) {
		this.listener = listener;
		btn1.setOnClickListener(listener);
		btn2.setOnClickListener(listener);
		btn3.setOnClickListener(listener);
		btn4.setOnClickListener(listener);
	}
}
