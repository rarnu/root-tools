package com.rarnu.vim.emotion.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;

import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.PageItem;
import com.rarnu.vim.emotion.utils.UIUtils;

public class GridPage4x4 extends RelativeLayout implements OnClickListener {

	public interface OnKeywordClickListener {
		void onKeywordClick(View v, Object extraData);
	}

	private ButtonEx[] btns = null;
	private OnKeywordClickListener listener;

	public GridPage4x4(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public GridPage4x4(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public GridPage4x4(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.face_page, null));

		btns = new ButtonEx[16];
		btns[0] = (ButtonEx) findViewById(R.id.btn1);
		btns[1] = (ButtonEx) findViewById(R.id.btn2);
		btns[2] = (ButtonEx) findViewById(R.id.btn3);
		btns[3] = (ButtonEx) findViewById(R.id.btn4);
		btns[4] = (ButtonEx) findViewById(R.id.btn5);
		btns[5] = (ButtonEx) findViewById(R.id.btn6);
		btns[6] = (ButtonEx) findViewById(R.id.btn7);
		btns[7] = (ButtonEx) findViewById(R.id.btn8);
		btns[8] = (ButtonEx) findViewById(R.id.btn9);
		btns[9] = (ButtonEx) findViewById(R.id.btn10);
		btns[10] = (ButtonEx) findViewById(R.id.btn11);
		btns[11] = (ButtonEx) findViewById(R.id.btn12);
		btns[12] = (ButtonEx) findViewById(R.id.btn13);
		btns[13] = (ButtonEx) findViewById(R.id.btn14);
		btns[14] = (ButtonEx) findViewById(R.id.btn15);
		btns[15] = (ButtonEx) findViewById(R.id.btn16);

		for (int i = 0; i < btns.length; i++) {
			btns[i].setOnClickListener(this);
		}

		resizeButtons();
	}

	private void resizeButtons() {
		// 3wx4h
		int width = UIUtils.getWidth();

		// int height = UIUtils.getHeight() - 96;

		width = width - 32;
		width = width / 35;

		for (int i = 0; i < btns.length; i++) {
			RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) btns[i]
					.getLayoutParams();
			rlp.width = width * 8;
			rlp.height = rlp.width * 5 / 4;
			rlp.leftMargin = (i % 4 == 0 ? 0 : width);
			rlp.topMargin = (i < 4 ? 0 : width);
			btns[i].setLayoutParams(rlp);
		}
	}

	public void setButtonClickEvent(OnKeywordClickListener listener) {
		this.listener = listener;
	}

	public void setButtonsItem(PageItem[] items) {
		int count = items.length;
		if (count > 16) {
			count = 16;
		}
		for (int i = 0; i < count; i++) {
			btns[i].setText(items[i].name);
			btns[i].setIcon(items[i].image);
			btns[i].setExtraData(items[i].data);
			if (items[i].name.equals("")) {
				btns[i].setVisibility(View.INVISIBLE);
			}
		}
	}

	@Override
	public void onClick(View v) {
		if (v instanceof ButtonEx) {
			if (listener != null) {
				listener.onKeywordClick(v, ((ButtonEx)v).getExtraData());
			}
		}
	}

}
