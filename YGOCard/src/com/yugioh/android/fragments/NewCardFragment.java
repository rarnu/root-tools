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
import com.yugioh.android.loader.NewCardLoader;

public class NewCardFragment extends BaseFragment implements
		OnItemClickListener, OnLoadCompleteListener<Cursor> {

	ListView lvNewCard;
	Cursor cNewCard;
	SimpleCursorAdapter adapterNewCard;
	NewCardLoader loaderNewcard;

	public NewCardFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_newcard;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_newcard;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_newcard;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initComponents() {
		lvNewCard = (ListView) innerView.findViewById(R.id.lvNewCard);
		loaderNewcard = new NewCardLoader(getActivity());
	}

	@Override
	protected void initEvents() {
		lvNewCard.setOnItemClickListener(this);
		loaderNewcard.registerListener(0, this);
	}

	@Override
	protected void initLogic() {
		loaderNewcard.startLoading();

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
		cNewCard.moveToPosition(position);
		int cardId = cNewCard.getInt(0);
		Intent inCardInfo = new Intent(getActivity(), CardInfoActivity.class);
		CardInfo info = YugiohUtils.getOneCard(getActivity(), cardId);
		inCardInfo.putExtra("cardinfo", info);
		startActivity(inCardInfo);

	}

	@Override
	public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
		if (data != null) {
			cNewCard = data;
			adapterNewCard = new SimpleCursorAdapter(getActivity(),
					R.layout.item_card, cNewCard, new String[] {
							FieldDefine.DataFields[5],
							FieldDefine.DataFields[10] }, new int[] {
							R.id.tvCardName, R.id.tvCardType },
					CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
			lvNewCard.setAdapter(adapterNewCard);
		}

	}

}
