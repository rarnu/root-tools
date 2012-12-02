package com.rarnu.zoe.love2.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.RelativeLayout;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.love2.utils.UIUtils;

public class RarnuGrid extends RelativeLayout {

	public static final int VIEW_MAIN_BIG = 0;
	public static final int VIEW_MAIN_1 = 1;
	public static final int VIEW_MAIN_2 = 2;
	public static final int VIEW_MAIN_3 = 3;
	public static final int VIEW_MAIN_4 = 4;

	RelativeLayout layMainBig, layMain1, layMain2, layMain3, layMain4,
			layGridSub;

	RelativeLayout[] laySubItem;
	OnClickListener listener = null;

	int itemWidth = 0, itemHeight = 0;

	public RarnuGrid(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public RarnuGrid(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public RarnuGrid(Context context) {
		super(context);
		init();
	}

	private void init() {
		setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));
		View v = inflate(getContext(), R.layout.comp_rarnu_grid, null);
		v.setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		addView(v);

		layMainBig = (RelativeLayout) findViewById(R.id.layMainBig);
		layMain1 = (RelativeLayout) findViewById(R.id.layMain1);
		layMain2 = (RelativeLayout) findViewById(R.id.layMain2);
		layMain3 = (RelativeLayout) findViewById(R.id.layMain3);
		layMain4 = (RelativeLayout) findViewById(R.id.layMain4);
		layGridSub = (RelativeLayout) findViewById(R.id.layGridSub);

		resize();
	}

	private void resize() {
		// -16-24
		itemWidth = (UIUtils.getWidth() - UIUtils.dipToPx(40)) / 4;
		// 200x160=5/4
		itemHeight = itemWidth * 4 / 5;
		// the big one
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) layMainBig
				.getLayoutParams();
		rlp.width = itemWidth * 2 + UIUtils.dipToPx(8);
		rlp.height = itemHeight * 2 + UIUtils.dipToPx(8);
		layMainBig.setLayoutParams(rlp);

		// 1~4
		rlp = (RelativeLayout.LayoutParams) layMain1.getLayoutParams();
		rlp.width = itemWidth;
		rlp.height = itemHeight;
		layMain1.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) layMain2.getLayoutParams();
		rlp.width = itemWidth;
		rlp.height = itemHeight;
		layMain2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) layMain3.getLayoutParams();
		rlp.width = itemWidth;
		rlp.height = itemHeight;
		layMain3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) layMain4.getLayoutParams();
		rlp.width = itemWidth;
		rlp.height = itemHeight;
		layMain4.setLayoutParams(rlp);

	}

	public void setMainView(int index, View v) {
		switch (index) {
		case VIEW_MAIN_BIG:
			setMainView(layMainBig, v);
			break;
		case VIEW_MAIN_1:
			setMainView(layMain1, v);
			break;
		case VIEW_MAIN_2:
			setMainView(layMain2, v);
			break;
		case VIEW_MAIN_3:
			setMainView(layMain3, v);
			break;
		case VIEW_MAIN_4:
			setMainView(layMain4, v);
			break;
		}
	}

	private void setMainView(RelativeLayout layout, View v) {
		v.setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		layout.addView(v);
	}

	public void setSubView(View[] v) {
		if (laySubItem != null) {
			laySubItem = null;
		}
		laySubItem = new RelativeLayout[v.length];
		int baseItemId = 100000;
		for (int i = 0; i < laySubItem.length; i++) {
			// 0123 4567 ...
			laySubItem[i] = new RelativeLayout(getContext());
			laySubItem[i].setId(baseItemId + i);
			RelativeLayout.LayoutParams rlp = new RelativeLayout.LayoutParams(
					itemWidth, itemHeight);
			rlp.topMargin = UIUtils.dipToPx(i > 3 ? 8 : 0);
			rlp.leftMargin = UIUtils.dipToPx(i % 4 == 0 ? 0 : 8);
			if (i % 4 != 0) {
				rlp.addRule(RelativeLayout.RIGHT_OF, baseItemId + i - 1);
			}
			if (i / 4 > 0) {
				rlp.addRule(RelativeLayout.BELOW, baseItemId + i - 4);
			}
			laySubItem[i].setLayoutParams(rlp);
			layGridSub.addView(laySubItem[i]);
			v[i].setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			laySubItem[i].addView(v[i]);
		}
	}
	
	public void setOnItemClickListener(OnClickListener listener) {
		this.listener = listener;
		layMainBig.setOnClickListener(listener);
		layMain1.setOnClickListener(listener);
		layMain2.setOnClickListener(listener);
		layMain3.setOnClickListener(listener);
		layMain4.setOnClickListener(listener);
		for (int i = 0; i < laySubItem.length; i++) {
			laySubItem[i].setOnClickListener(listener);
		}
	}

	public void setMainTag(int index, Integer tag) {
		switch (index) {
		case 0:
			layMainBig.setTag(tag);
			break;
		case 1:
			layMain1.setTag(tag);
			break;
		case 2:
			layMain2.setTag(tag);
			break;
		case 3:
			layMain3.setTag(tag);
			break;
		case 4:
			layMain4.setTag(tag);
			break;
		}
	}
	
	public void setSubTag(int index, Integer tag) {
		laySubItem[index].setTag(tag);
	}
}
