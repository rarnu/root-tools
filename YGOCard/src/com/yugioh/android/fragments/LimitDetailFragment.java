package com.yugioh.android.fragments;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.database.Cursor;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.CursorAdapter;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.CardInfoActivity;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.FieldDefine;
import com.yugioh.android.loader.LimitLoader;

public class LimitDetailFragment extends BaseFragment implements
		OnItemClickListener, OnLoadCompleteListener<Cursor> {

	private int detailType;

	ListView lvLimitCard;
	Cursor cLimit;
	SimpleCursorAdapter adapterLimit;
	LimitLoader loaderLimit;

	public LimitDetailFragment(String tagText, String tabTitle, int detailType) {
		super(tagText, tabTitle);
		this.detailType = detailType;
	}

	@Override
	protected int getBarTitle() {
		return R.string.lm_banned;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.lm_banned;
	}

	@Override
	protected String getCustomTitle() {
		return null;
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_limit_detail;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initComponents() {
		lvLimitCard = (ListView) innerView.findViewById(R.id.lvLimitCard);
		loaderLimit = new LimitLoader(getActivity(), detailType);
	}

	@Override
	protected void initEvents() {
		lvLimitCard.setOnItemClickListener(this);
		loaderLimit.registerListener(0, this);
	}

	@Override
	protected void initLogic() {
		loaderLimit.startLoading();
	}

	@Override
	protected void initMenu(Menu arg0) {

	}

	@Override
	protected void onGetNewArguments(Bundle arg0) {

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		cLimit.moveToPosition(position);
		int cardId = cLimit.getInt(0);
		Intent inCardInfo = new Intent(getActivity(), CardInfoActivity.class);
		CardInfo info = YugiohUtils.getOneCard(getActivity(), cardId);
		inCardInfo.putExtra("cardinfo", info);
		startActivity(inCardInfo);
	}

	@Override
	public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
		if (data != null) {
			cLimit = data;
			adapterLimit = new SimpleCursorAdapter(getActivity(),
					R.layout.item_card, cLimit, new String[] {
							FieldDefine.DataFields[5],
							FieldDefine.DataFields[10] }, new int[] {
							R.id.tvCardName, R.id.tvCardType },
					CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
			lvLimitCard.setAdapter(adapterLimit);
		}

	}

}
