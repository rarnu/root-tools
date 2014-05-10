package com.yugioh.android.fragments;

import android.content.Loader;
import android.database.Cursor;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.loader.SearchLoader;
import com.yugioh.android.utils.MiscUtils;

public class DeckCardFragment extends BaseFragment implements Loader.OnLoadCompleteListener<Cursor>, AdapterView.OnItemClickListener {

    ListView lvCards;
    TextView tvListNoCard;
    SearchLoader loader;
    Cursor cSearchResult;
    SimpleCursorAdapter adapterSearchResult;

    public DeckCardFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return getArguments().getString("name");
    }

    @Override
    public void initComponents() {
        lvCards = (ListView) innerView.findViewById(R.id.lvCards);
        tvListNoCard = (TextView) innerView.findViewById(R.id.tvListNoCard);
        loader = new SearchLoader(getActivity(), getArguments());
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvCards.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {
        tvListNoCard.setText(R.string.list_nocard_searching);
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_deck_cards;
    }

    @Override
    public String getMainActivityName() {
        return null;
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
    public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
        if (data != null) {
            cSearchResult = data;
            adapterSearchResult = new SimpleCursorAdapter(getActivity(), R.layout.item_card, cSearchResult, new String[]{"name", "sCardType"}, new int[]{R.id.tvCardName, R.id.tvCardType}, CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);

        }
        if (getActivity() != null) {
            lvCards.setAdapter(adapterSearchResult);
            tvListNoCard.setVisibility(adapterSearchResult.getCount() == 0 ? View.VISIBLE : View.GONE);
            tvListNoCard.setText(R.string.deck_nocard);
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        MiscUtils.openCardDetail(getActivity(), cSearchResult, position);
    }
}
