package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RelativeLayout;

import com.rarnu.root.pp4.R;
import com.rarnu.tools.root.utils.UIUtils;

public class SearchBar extends RelativeLayout {

	// [region] field define
	private EditText etSearch;
	private Button btnCancel;
	private Button btnAdd;
	// [/region]
	
	// [region] constructor
	public SearchBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}
	
	public SearchBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}
	
	public SearchBar(Context context) {
		super(context);
		init();
	}
	// [/region]
	
	// [region] common
	public void init() {
		addView(inflate(getContext(), R.layout.search_bar, null));
		etSearch = (EditText) findViewById(R.id.etSearch);
		btnCancel = (Button) findViewById(R.id.btnCancel);
		btnAdd = (Button) findViewById(R.id.btnAdd);
	}
	
	public CharSequence getText() {
		return etSearch.getText();
	}
	
	public void setText(CharSequence text) {
		etSearch.setText(text);
	}
	
	public Button getCancelButton() {
		return btnCancel;
	}
	
	public Button getAddButton() {
		return btnAdd;
	}
	
	public EditText getEditText() {
		return etSearch;
	}
	
	public void setAddButtonVisible(boolean visible) {
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams)btnAdd.getLayoutParams();
		rlp.width = (visible ? UIUtils.dipToPx(40): 0);
		btnAdd.setLayoutParams(rlp);
		btnAdd.setEnabled(visible);
//		btnAdd.setVisibility(visible ? View.VISIBLE: View.GONE);
	}

	// [/region]
}
