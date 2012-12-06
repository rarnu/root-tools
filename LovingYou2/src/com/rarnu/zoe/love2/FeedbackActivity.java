package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

// TODO: feedback
public class FeedbackActivity extends BaseActivity implements OnClickListener {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_feedback);
	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.contact_us);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		}

	}

}
