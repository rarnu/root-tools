package com.rarnu.findaround.comp;

import com.rarnu.findaround.R;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.RelativeLayout;

public class GridPageSearch extends RelativeLayout {

	LineEditText etSearch;

	public GridPageSearch(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public GridPageSearch(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public GridPageSearch(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.welcome_page_search, null));
		etSearch = (LineEditText) findViewById(R.id.etSearch);
	}

	public LineEditText getEdit() {
		return etSearch;
	}
}
