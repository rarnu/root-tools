package com.yugioh.android.fragments;

import android.content.Loader;
import android.database.Cursor;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardItems;
import com.yugioh.android.common.MenuIds;
import com.yugioh.android.loader.SearchLoader;
import com.yugioh.android.utils.MiscUtils;

public class PackageCardsFragment extends BaseFragment implements Loader.OnLoadCompleteListener<Cursor>, AdapterView.OnItemClickListener {

    ListView lvCards;
    TextView tvListNoCard;
    TextView tvLoading;
    SearchLoader loader;
    Cursor cSearchResult;
    SimpleCursorAdapter adapterSearchResult;
    MenuItem itemRefresh;
    private Handler hPack = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                tvLoading.setVisibility(View.GONE);
                itemRefresh.setEnabled(true);
                lvCards.setEnabled(true);
                CardItems items = (CardItems) msg.obj;
                Bundle bn = new Bundle();
                bn.putIntArray("ids", items.cardIds);
                loader.setBundle(bn);
                loader.startLoading();
            }
            super.handleMessage(msg);
        }
    };

    public PackageCardsFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.package_cards);
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
        return getArguments().getString("pack");
    }

    @Override
    public void initComponents() {
        lvCards = (ListView) innerView.findViewById(R.id.lvCards);
        tvListNoCard = (TextView) innerView.findViewById(R.id.tvListNoCard);
        loader = new SearchLoader(getActivity(), getArguments());
        tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
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
        return R.layout.fragment_package_cards;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        itemRefresh = menu.add(0, MenuIds.MENUID_REFRESH, 99, R.string.refresh);
        itemRefresh.setIcon(android.R.drawable.ic_menu_revert);
        itemRefresh.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuIds.MENUID_REFRESH:
                tvLoading.setVisibility(View.VISIBLE);
                itemRefresh.setEnabled(false);
                lvCards.setEnabled(false);
                MiscUtils.loadCardsDataT(0, getArguments().getString("id"), hPack, true);
                break;
        }
        return true;
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
            tvListNoCard.setText(R.string.package_nocard);
        }
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        MiscUtils.openCardDetail(getActivity(), cSearchResult, position);
    }
}
