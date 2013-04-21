package com.yugioh.android.fragments;

import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.CursorAdapter;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.CardInfoActivity;
import com.yugioh.android.FragmentNames;
import com.yugioh.android.Fragments;
import com.yugioh.android.R;
import com.yugioh.android.database.YugiohUtils;
import com.yugioh.android.define.FieldDefine;

public class SearchResultFragment extends BaseFragment implements
		OnItemClickListener {

	Cursor cSearchResult;
	SimpleCursorAdapter adapterSearchResult;

	ListView lvList;
	TextView tvListNoCard;

	public SearchResultFragment(String tagText, String tabTitle) {
		super(tagText, tabTitle);
	}

	@Override
	protected int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {
		lvList = (ListView) innerView.findViewById(R.id.lvList);
		tvListNoCard = (TextView) innerView.findViewById(R.id.tvListNoCard);
	}

	@Override
	protected void initEvents() {
		lvList.setOnItemClickListener(this);
	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_search_result;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

		String cardType = bn.getString("cardType");
		String cardAttribute = bn.getString("cardAttribute");
		int cardLevel = bn.getInt("cardLevel");
		String cardRace = bn.getString("cardRace");
		String cardName = bn.getString("cardName");
		String cardEffect = bn.getString("cardEffect");
		String cardAtk = bn.getString("cardAtk");
		String cardDef = bn.getString("cardDef");
		String cardRare = bn.getString("cardRare");
		String cardBelongs = bn.getString("cardBelongs");
		String cardLimit = bn.getString("cardLimit");
		int cardTunner = bn.getInt("cardTunner");

		cSearchResult = YugiohUtils.getCards(getActivity(), cardType,
				cardAttribute, cardLevel, cardRace, cardName, cardEffect,
				cardAtk, cardDef, cardRare, cardBelongs, cardLimit, cardTunner);

		adapterSearchResult = new SimpleCursorAdapter(getActivity(),
				R.layout.item_card, cSearchResult,
				new String[] { FieldDefine.DataFields[5],
						FieldDefine.DataFields[10] }, new int[] {
						R.id.tvCardName, R.id.tvCardType },
				CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
		lvList.setAdapter(adapterSearchResult);
		tvListNoCard
				.setVisibility(adapterSearchResult.getCount() == 0 ? View.VISIBLE
						: View.GONE);

		((MainFragment) Fragments.getFragment(getActivity(),
				FragmentNames.FRAGMENT_MAIN)).setTabPosition(1);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		cSearchResult.moveToPosition(position);
		int cardId = cSearchResult.getInt(0);
		Intent inCardInfo = new Intent(getActivity(), CardInfoActivity.class);
		inCardInfo.putExtra("CardId", cardId);
		startActivity(inCardInfo);
	}

	@Override
	protected String getCustomTitle() {
		return null;
	}

}
