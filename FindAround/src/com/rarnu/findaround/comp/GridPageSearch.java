package com.rarnu.findaround.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.RelativeLayout;

import com.rarnu.findaround.R;

public class GridPageSearch extends RelativeLayout {

	EditText etSearch;
	ListView lvHistory;

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
		etSearch = (EditText) findViewById(R.id.etSearch);
		lvHistory = (ListView) findViewById(R.id.lvHistory);
	}

	public EditText getEdit() {
		return etSearch;
	}

	public ListView getListView() {
		return lvHistory;
	}
}
