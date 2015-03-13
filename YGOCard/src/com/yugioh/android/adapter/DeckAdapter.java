package com.yugioh.android.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import com.rarnu.devlib.adapter.BaseAdapter;
import com.yugioh.android.R;
import com.yugioh.android.classes.DeckItem;

import java.util.List;

public class DeckAdapter extends BaseAdapter<DeckItem> {

    public DeckAdapter(Context context, List<DeckItem> list) {
        super(context, list);
    }

    @Override
    public String getValueText(DeckItem item) {
        return item.name + item.type;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.item_deck, parent, false);
        }
        DeckHolder holder = (DeckHolder) v.getTag();
        if (holder == null) {
            holder = new DeckHolder();
            holder.tvDeckName = (TextView) v.findViewById(R.id.tvDeckName);
            holder.tvDeckType = (TextView) v.findViewById(R.id.tvDeckType);
            v.setTag(holder);
        }
        DeckItem item = list.get(position);
        if (item != null) {
            holder.tvDeckName.setText(item.name);
            holder.tvDeckType.setText(item.type);
        }
        return v;
    }
}
