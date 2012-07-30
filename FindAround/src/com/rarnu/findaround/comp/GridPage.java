package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Gallery;
import android.widget.RelativeLayout;

import com.rarnu.findaround.R;

public class GridPage extends RelativeLayout implements OnClickListener {

	private TextViewEx[] btns = null;
	private OnClickListener listener;

	public GridPage(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public GridPage(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public GridPage(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.welcome_page_item, null));

		btns = new TextViewEx[12];
		btns[0] = (TextViewEx) findViewById(R.id.btn1);
		btns[1] = (TextViewEx) findViewById(R.id.btn2);
		btns[2] = (TextViewEx) findViewById(R.id.btn3);
		btns[3] = (TextViewEx) findViewById(R.id.btn4);
		btns[4] = (TextViewEx) findViewById(R.id.btn5);
		btns[5] = (TextViewEx) findViewById(R.id.btn6);
		btns[6] = (TextViewEx) findViewById(R.id.btn7);
		btns[7] = (TextViewEx) findViewById(R.id.btn8);
		btns[8] = (TextViewEx) findViewById(R.id.btn9);
		btns[9] = (TextViewEx) findViewById(R.id.btn10);
		btns[10] = (TextViewEx) findViewById(R.id.btn11);
		btns[11] = (TextViewEx) findViewById(R.id.btn12);

		for (int i = 0; i < btns.length; i++) {
			btns[i].setOnClickListener(this);
		}

	}

	public void setGallery(Gallery g) {
		for (int i = 0; i < btns.length; i++) {
			((TextViewEx) btns[i]).setGallery(g);
		}
	}

	public void setButtonClickEvent(OnClickListener listener) {
		this.listener = listener;
	}

	public void setButtonsText(String[] text) {
		int count = text.length;
		if (count > 12) {
			count = 12;
		}
		for (int i = 0; i < count; i++) {
			btns[i].setText(text[i]);
		}
	}

	public void setButtonsText(int[] text) {
		int count = text.length;
		if (count > 12) {
			count = 12;
		}
		for (int i = 0; i < count; i++) {
			btns[i].setText(text[i]);
		}
	}

	@Override
	public void onClick(View v) {
		if (listener != null) {
			listener.onClick(v);
		}
	}

}
