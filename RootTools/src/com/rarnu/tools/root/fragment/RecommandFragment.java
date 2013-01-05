package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;

import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.base.BaseFragment;
import com.rarnu.tools.root.utils.ApkUtils;

public class RecommandFragment extends BaseFragment implements OnClickListener {

	RelativeLayout layAnjuke, layHaozu, layXinfang, layBroker;

	private static final String NS_ANJUKE = "com.anjuke.android.app";
	private static final String AS_WELCOME_ANJUKE = ".activity.WelcomeActivity";
	
	private static final String NS_HAOZU = "com.anjuke.android.haozu";
	private static final String AS_WELCOME_HAOZU = ".activity.WelcomeActivity";
	
	private static final String NS_XINFANG = "com.anjuke.android.xinfang";
	private static final String AS_WELCOME_XINFANG = ".activity.WelcomeActivity";
	
	private static final String NS_BROKER = "com.anjuke.android.newbroker";
	private static final String AS_WELCOME_BROKER = ".activity.LoginActivity";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		LogApi.logEnterAppRecommand();
	}

	@Override
	protected int getBarTitle() {
		return R.string.short_recommand;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.short_recommand_with_path;
	}

	@Override
	protected void initComponents() {
		layAnjuke = (RelativeLayout) innerView.findViewById(R.id.layAnjuke);
		layHaozu = (RelativeLayout) innerView.findViewById(R.id.layHaozu);
		layXinfang = (RelativeLayout) innerView.findViewById(R.id.layXinfang);
		layBroker = (RelativeLayout) innerView.findViewById(R.id.layBroker);
		layAnjuke.setOnClickListener(this);
		layHaozu.setOnClickListener(this);
		layXinfang.setOnClickListener(this);
		layBroker.setOnClickListener(this);

	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.layout_recommand;
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {

		case R.id.layAnjuke:
			ApkUtils.gotoApp(getActivity(), NS_ANJUKE, AS_WELCOME_ANJUKE);
			break;
		case R.id.layHaozu:
			ApkUtils.gotoApp(getActivity(), NS_HAOZU, AS_WELCOME_HAOZU);
			break;
		case R.id.layXinfang:
			ApkUtils.gotoApp(getActivity(), NS_XINFANG, AS_WELCOME_XINFANG);
			break;
		case R.id.layBroker:
			ApkUtils.gotoApp(getActivity(), NS_BROKER, AS_WELCOME_BROKER);
			break;
		}

		getActivity().finish();

	}

}
