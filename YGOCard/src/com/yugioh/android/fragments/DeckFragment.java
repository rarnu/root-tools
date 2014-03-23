package com.yugioh.android.fragments;

import android.content.Intent;
import android.content.Loader;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.DeckCardActivity;
import com.yugioh.android.R;
import com.yugioh.android.adapter.DeckAdapter;
import com.yugioh.android.classes.CardItems;
import com.yugioh.android.classes.DeckItem;
import com.yugioh.android.loader.DeckLoader;
import com.yugioh.android.utils.MiscUtils;

import java.util.ArrayList;
import java.util.List;

public class DeckFragment extends BaseFragment implements AdapterView.OnItemClickListener, Loader.OnLoadCompleteListener<List<DeckItem>> {

    DeckAdapter adapter;
    List<DeckItem> list;
    ListView lvDeck;
    TextView tvListNoDeck;
    TextView tvLoading;
    DeckLoader loader;
    private Handler hDeck = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                lvDeck.setEnabled(true);
                tvLoading.setVisibility(View.GONE);
                Bundle bn = new Bundle();
                CardItems items = (CardItems) msg.obj;
                if (items != null) {
                    bn.putIntArray("ids", items.cardIds);
                    bn.putString("pack", items.packageName);
                    Intent inCards = new Intent(getActivity(), DeckCardActivity.class);
                    inCards.putExtras(bn);
                    startActivity(inCards);
                } else {
                    Toast.makeText(getActivity(), R.string.package_cannot_load, Toast.LENGTH_LONG).show();
                }
            }
            super.handleMessage(msg);
        }
    };

    public DeckFragment() {
        super();
    }

    @Override
    public int getBarTitle() {
        return R.string.lm_deck;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.lm_deck;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_deck;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        lvDeck = (ListView) innerView.findViewById(R.id.lvDeck);
        tvListNoDeck = (TextView) innerView.findViewById(R.id.tvListNoDeck);
        tvLoading = (TextView) innerView.findViewById(R.id.tvLoading);
        loader = new DeckLoader(getActivity());
        list = new ArrayList<DeckItem>();
        adapter = new DeckAdapter(getActivity(), list);
        lvDeck.setAdapter(adapter);
    }

    @Override
    public void initEvents() {
        loader.registerListener(0, this);
        lvDeck.setOnItemClickListener(this);
    }

    @Override
    public void initLogic() {
        tvLoading.setVisibility(View.VISIBLE);
        tvListNoDeck.setText(R.string.list_nocard_searching);
        loader.startLoading();
    }

    @Override
    public void initMenu(Menu arg0) {

    }

    @Override
    public void onGetNewArguments(Bundle arg0) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        lvDeck.setEnabled(false);
        tvLoading.setVisibility(View.VISIBLE);
        DeckItem item = list.get(position);
        MiscUtils.loadCardsDataT(1, item.id, hDeck, false);
    }

    @Override
    public void onLoadComplete(Loader<List<DeckItem>> loader, List<DeckItem> data) {
        list.clear();
        if (data != null) {
            list.addAll(data);

        }
        if (getActivity() != null) {
            adapter.setNewList(list);
            tvLoading.setVisibility(View.GONE);
            tvListNoDeck.setVisibility(list.size() == 0 ? View.VISIBLE : View.GONE);
            tvListNoDeck.setText(R.string.deck_nocard);
        }
    }
}
