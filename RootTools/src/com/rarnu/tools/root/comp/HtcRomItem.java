package com.rarnu.tools.root.comp;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.utils.UIUtils;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.CheckBox;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class HtcRomItem extends RelativeLayout {

	TextView itemName, itemDesc;
	CheckBox chkSelect;

	public HtcRomItem(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public HtcRomItem(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public HtcRomItem(Context context) {
		super(context);
		init();
	}

	private void init() {
		addView(inflate(getContext(), R.layout.htcrom_item, null));
		setLayoutParams(new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, UIUtils.dipToPx(56)));

		itemName = (TextView) findViewById(R.id.itemName);
		itemDesc = (TextView) findViewById(R.id.itemDesc);
		chkSelect = (CheckBox) findViewById(R.id.chkSelect);
	}

	public void setName(String text) {
		itemName.setText(text);
	}

	public void setName(int resid) {
		itemName.setText(resid);
	}

	public void setDesc(String text) {
		itemDesc.setText(text);
	}

	public void setDesc(int resid) {
		itemDesc.setText(resid);
	}

	public void setChecked(boolean checked) {
		chkSelect.setChecked(checked);
	}

	public boolean isChecked() {
		return chkSelect.isChecked();
	}

	public void enable() {
		chkSelect.setEnabled(true);
	}

	public void disable() {
		chkSelect.setEnabled(false);
	}
}
