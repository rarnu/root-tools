package com.rarnu.adcenter.fragment;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ScrollView;
import android.widget.TextView;

import com.rarnu.adcenter.R;
import com.rarnu.adcenter.classes.CashItem;
import com.rarnu.adcenter.classes.UserItem;
import com.rarnu.adcenter.database.AdUtils;
import com.rarnu.adcenter.loader.CashLoader;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;

public class CashFragment extends BaseFragment implements
		OnLoadCompleteListener<CashItem>, OnClickListener {

	CashLoader loader;
	TextView tvLoading;
	TextView tvNotLogin;
	ScrollView svCash;
	UserItem user;
	CashItem cash;
	TextView tvCashValue;
	TextView tvMoneyValue;
	TextView tvRateValue;
	Button btnExchage;

	public CashFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_cash);
	}

	@Override
	public int getBarTitle() {
		return R.string.user_cash;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.user_cash;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		svCash = (ScrollView) innerView.findViewById(R.id.svCash);
		tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
		tvNotLogin = (TextView) innerView.findViewById(R.id.tvNotLogin);
		tvCashValue = (TextView) innerView.findViewById(R.id.tvCashValue);
		tvMoneyValue = (TextView) innerView.findViewById(R.id.tvMoneyValue);
		tvRateValue = (TextView) innerView.findViewById(R.id.tvRateValue);
		btnExchage = (Button) innerView.findViewById(R.id.btnExchage);
		loader = new CashLoader(getActivity());
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
		btnExchage.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		user = AdUtils.queryUser(getActivity());
		if (user != null) {
			svCash.setVisibility(View.VISIBLE);
			tvNotLogin.setVisibility(View.GONE);
			tvLoading.setVisibility(View.VISIBLE);

			loader.setUserId(user.id);
			loader.startLoading();

		} else {
			svCash.setVisibility(View.GONE);
			tvNotLogin.setVisibility(View.VISIBLE);
		}
	}

	private void loadCashData() {
		if (cash != null) {
			tvCashValue.setText(String.valueOf(cash.cash));
			tvMoneyValue.setText(String.valueOf(cash.real_money));
			tvRateValue.setText(String.valueOf(cash.rate));
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_cash;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onLoadComplete(Loader<CashItem> loader, CashItem data) {
		if (data != null) {
			cash = data;
		}

		if (getActivity() != null) {
			loadCashData();
			tvLoading.setVisibility(View.GONE);
		}

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnExchage:
			// TODO: exchage money
			break;
		}

	}

}
