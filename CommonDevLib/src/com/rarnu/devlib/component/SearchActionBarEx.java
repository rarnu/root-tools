package com.rarnu.devlib.component;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.rarnu.devlib.R;
import com.rarnu.utils.UIUtils;

/**
 * Created by rarnu on 2/6/15.
 */
public class SearchActionBarEx extends RelativeLayout implements View.OnClickListener, View.OnLongClickListener {

    @Override
    public boolean onLongClick(View v) {
        String tag = (String) v.getTag();
        if (tag != null && !tag.equals("")) {
            // toast tag
        }
        return true;
    }

    public interface SearchActionBarExListener {
        void onActionButtonClicked(SearchActionBarEx bar, int buttonId);

        void onActiontextChanged(SearchActionBarEx bar, String text);
    }

    private SearchViewEx etSearch;
    private ImageButton btnMostLeft;
    private ImageButton btnMostRight;
    private SearchActionBarExListener listener;

    public static final int ID_BTN_MOST_LEFT = 10001;
    public static final int ID_BTN_MOST_RIGHT = 10002;
    public static final int ID_EDIT_SEARCH = 10003;

    public SearchActionBarEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init();
    }

    public SearchActionBarEx(Context context, AttributeSet attrs) {
        super(context, attrs);
        init();
    }

    public SearchActionBarEx(Context context) {
        super(context);
        init();
    }

    private void init() {

        btnMostLeft = new ImageButton(getContext());
        btnMostLeft.setId(ID_BTN_MOST_LEFT);
        LayoutParams rllpMostLeft = new LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpMostLeft.addRule(RelativeLayout.ALIGN_PARENT_LEFT, RelativeLayout.TRUE);
        btnMostLeft.setLayoutParams(rllpMostLeft);
        btnMostLeft.setBackgroundResource(R.drawable.btn_cab_done);
        btnMostLeft.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnMostLeft.setVisibility(View.GONE);
        addView(btnMostLeft);

        btnMostRight = new ImageButton(getContext());
        btnMostRight.setId(ID_BTN_MOST_RIGHT);
        LayoutParams rllpMostRight = new LayoutParams(UIUtils.dipToPx(48), LayoutParams.MATCH_PARENT);
        rllpMostRight.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, RelativeLayout.TRUE);
        btnMostRight.setLayoutParams(rllpMostRight);
        btnMostRight.setBackgroundResource(R.drawable.btn_cab_done);
        btnMostRight.setScaleType(ImageView.ScaleType.CENTER_INSIDE);
        btnMostRight.setVisibility(View.GONE);
        addView(btnMostRight);

        etSearch = new SearchViewEx(getContext());
        etSearch.setId(ID_EDIT_SEARCH);
        LayoutParams rllpSearch = new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
        rllpSearch.addRule(RelativeLayout.RIGHT_OF, ID_BTN_MOST_LEFT);
        rllpMostLeft.addRule(RelativeLayout.LEFT_OF, ID_BTN_MOST_RIGHT);
        etSearch.setLayoutParams(rllpSearch);
        addView(etSearch);

        btnMostLeft.setOnClickListener(this);
        btnMostRight.setOnClickListener(this);
        btnMostLeft.setOnLongClickListener(this);
        btnMostRight.setOnLongClickListener(this);
        etSearch.setCancelButtonListener(this);

        etSearch.getEditor().addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (listener != null) {
                    listener.onActiontextChanged(SearchActionBarEx.this, s.toString());
                }
            }

            @Override
            public void afterTextChanged(Editable s) {

            }
        });

    }

    public void setActionBarListener(SearchActionBarExListener l) {
        listener = l;
    }

    @Override
    public void onClick(View v) {
        if (listener != null) {
            if (v instanceof ImageButton) {
                listener.onActionButtonClicked(this, v.getId());
            }
        }
    }

    public void addButton(int position, int title, int icon) {
        addButton(position, getResources().getString(title), getResources().getDrawable(icon));
    }

    public void addButton(int position, String title, int icon) {
        addButton(position, title, getResources().getDrawable(icon));
    }

    public void addButton(int position, int title, Drawable icon) {
        addButton(position, getResources().getString(title), icon);
    }

    public void addButton(int position, String title, Drawable icon) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setTag(title);
                btnMostLeft.setImageDrawable(icon);
                btnMostLeft.setVisibility(View.VISIBLE);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setTag(title);
                btnMostRight.setImageDrawable(icon);
                btnMostRight.setVisibility(View.VISIBLE);
                break;
        }
    }

    public void removeButton(int position) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setVisibility(View.GONE);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setVisibility(View.GONE);
                break;
        }
    }

    public void setButtonEnabled(int position, boolean e) {
        switch (position) {
            case ID_BTN_MOST_LEFT:
                btnMostLeft.setEnabled(e);
                break;
            case ID_BTN_MOST_RIGHT:
                btnMostRight.setEnabled(e);
                break;
        }
    }

    public void setSearchText(String text) {
        etSearch.setText(text);
    }
}
