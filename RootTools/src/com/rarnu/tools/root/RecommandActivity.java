package com.rarnu.tools.root;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.widget.RelativeLayout;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseActivity;
import com.rarnu.tools.root.utils.ApkUtils;

public class RecommandActivity extends BaseActivity implements OnClickListener {

	// [region] field define

	RelativeLayout layAnjuke, layHaozu, layBroker;
	// [/region]

	// [region] const define
	private static final String NS_ANJUKE = "com.anjuke.android.app";
	private static final String NS_HAOZU = "com.anjuke.android.haozu";
	private static final String NS_BROKER = "com.anjuke.android.newbroker";
	private static final String AS_WELCOME_ANJUKE = ".activity.WelcomeActivity";
	private static final String AS_WELCOME_HAOZU = ".activity.WelcomeActivity";
	private static final String AS_WELCOME_BROKER = ".activity.WelcomeActivity";

	// [/region]

	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.layout_recommand);
		init();
		LogApi.logEnterAppRecommand();
	}

	// [/region]

	// [region] events

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.layAnjuke:
			ApkUtils.gotoApp(this, NS_ANJUKE, AS_WELCOME_ANJUKE);
			finish();
			break;
		case R.id.layHaozu:
			ApkUtils.gotoApp(this, NS_HAOZU, AS_WELCOME_HAOZU);
			finish();
			break;
		case R.id.layBroker:
			ApkUtils.gotoApp(this, NS_BROKER, AS_WELCOME_BROKER);
			finish();
			break;
		}
	}

	// [/region]

	// [region] init

	@Override
	public void init() {
		mappingTitle();
		mappingComp();
		initSearchBar();
		initTitle();
		initEvents();

	}

	@Override
	public void mappingComp() {

		layAnjuke = (RelativeLayout) findViewById(R.id.layAnjuke);
		layHaozu = (RelativeLayout) findViewById(R.id.layHaozu);
		layBroker = (RelativeLayout) findViewById(R.id.layBroker);

	}

	@Override
	public void initTitle() {
		tvName.setText(R.string.app_push);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);

		// tbTitle.setText(getString(R.string.app_push));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);
	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);
		layAnjuke.setOnClickListener(this);
		layHaozu.setOnClickListener(this);
		layBroker.setOnClickListener(this);

	}
	// [/region]
}
