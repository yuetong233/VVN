# app.py — VA Cost of Living (Dash, no GeoPandas required)
import json, re
from pathlib import Path
import pandas as pd
import dash
from dash import dcc, html, Input, Output
import plotly.express as px

# ---------- CONFIG (edit these to match your setup) ----------
DATA_DIR = Path("/Users/wheeinner/Library/Mobile Documents/com~apple~CloudDocs/DSPG/VVN/Cost_of_Living_Maps/CoL_Data")
GEOJSON_FILE = DATA_DIR / "va_counties.geojson"     # change if your file name differs
# ------------------------------------------------------------

FAMILY_STRUCTURES = [
    "1 Adult: 19–64 Years",
    "2 Adults: 19–64 Years",
    "1 Adult + 1 Child",
    "2 Adults + 2 Children",
    "1 Adult: 65+",
    "2 Adults: 65+"
]
COST_VARIABLES = ["Housing","Food","Transportation","Taxes","Healthcare","Childcare","Technology","Elder Care","Miscellaneous"]

CSV_FILES = {
    "min": {
        "Elder Care": "minimum_elder_care_cost.csv",
        "Transportation": "minimum_transportation_data.csv",
        "Technology": "minimum_technology_costs.csv",
        "Food": "final_minimum_food_data.csv",
        "Taxes": "minimum_tax_cost.csv",
        "Childcare": "childcare_minimum_cost.csv",
        "Housing": "minimum_housing_cost.csv",
        "Healthcare": "minimum_healthcare_cost.csv",
    },
    "avg": {
        "Elder Care": "average_elder_care_cost.csv",
        "Transportation": "average_transportation_data.csv",
        "Technology": "average_technology_costs.csv",
        "Food": "final_average_food_data.csv",
        "Taxes": "average_tax_cost.csv",
        "Childcare": "childcare_average_cost.csv",
        "Housing": "average_housing_cost.csv",
        "Healthcare": "average_healthcare_cost.csv",
    },
}

# ----- helpers -----
FAMILY_PATTERNS = [
    (re.compile(r"1.*Adult.*19.*64|1.*Adult.*Working|Working.*Adult", re.I), "1 Adult: 19–64 Years"),
    (re.compile(r"2.*Adults.*19.*64|2.*Adults.*Working|Two.*Adults.*Working", re.I), "2 Adults: 19–64 Years"),
    (re.compile(r"1.*Adult.*1.*Child|Adult.*Child|Single.*Parent", re.I), "1 Adult + 1 Child"),
    (re.compile(r"2.*Adults.*2.*Children|Family.*4|Typical.*Family", re.I), "2 Adults + 2 Children"),
    (re.compile(r"1.*Adult.*65|Senior.*Individual|Elder.*Individual", re.I), "1 Adult: 65+"),
    (re.compile(r"2.*Adults.*65|Senior.*Couple|Elder.*Couple", re.I), "2 Adults: 65+"),
]
def std_family_header(h):
    for rgx, std in FAMILY_PATTERNS:
        if rgx.search(h or ""): return std
    return None

def clean_number(x):
    if pd.isna(x): return None
    if isinstance(x, (int,float)): return float(x)
    try: return float(str(x).replace("$","").replace(",","").strip())
    except: return None

def detect_county_col(cols):
    for k in ["County","county","NAME","Name","name","County_Name","county_name"]:
        if k in cols: return k
    for c in cols:
        if re.search(r"county|name", c, re.I): return c
    return None

def load_cost_table(csv_path: Path) -> pd.DataFrame:
    if not csv_path.exists(): return pd.DataFrame()
    df = pd.read_csv(csv_path)
    if df.empty: return df
    county_col = detect_county_col(df.columns)
    if not county_col: return pd.DataFrame()
    out = pd.DataFrame({"County": df[county_col].astype(str).str.strip()})
    for col in df.columns:
        std = std_family_header(col)
        if std:
            out[std] = df[col].map(clean_number)
    keep = ["County"] + [c for c in FAMILY_STRUCTURES if c in out.columns]
    out = out[keep].dropna(subset=["County"]).drop_duplicates()
    return out

def build_long_for_type(type_key, costvar_to_file):
    rows = []
    for cost_var, fname in costvar_to_file.items():
        wide = load_cost_table(DATA_DIR / fname)
        if wide.empty: continue
        for fs in [c for c in FAMILY_STRUCTURES if c in wide.columns]:
            tmp = wide[["County", fs]].rename(columns={fs:"Cost"}).copy()
            tmp["FamilyStructure"] = fs
            tmp["CostVariable"] = cost_var
            tmp["Type"] = type_key
            tmp = tmp.loc[tmp["Cost"].notna() & (tmp["Cost"] > 0)]
            rows.append(tmp)
    return pd.concat(rows, ignore_index=True) if rows else pd.DataFrame(
        columns=["County","Cost","FamilyStructure","CostVariable","Type"]
    )

def add_miscellaneous(df_long: pd.DataFrame) -> pd.DataFrame:
    if df_long.empty: return df_long
    grp = (df_long[df_long["CostVariable"]!="Taxes"]
           .groupby(["County","FamilyStructure","Type"], as_index=False)["Cost"].sum()
           .rename(columns={"Cost":"subtotal"}))
    misc = grp.copy()
    misc["CostVariable"] = "Miscellaneous"
    misc["Cost"] = 0.10 * misc["subtotal"]
    misc = misc.drop(columns=["subtotal"])
    return pd.concat([df_long, misc], ignore_index=True)

def build_totals_and_breakdown(df_long: pd.DataFrame):
    if df_long.empty: return pd.DataFrame(), pd.DataFrame()
    cat_agg = df_long.groupby(["County","FamilyStructure","Type","CostVariable"], as_index=False)["Cost"].sum()
    totals = cat_agg.groupby(["County","FamilyStructure","Type"], as_index=False)["Cost"].sum().rename(columns={"Cost":"Total"})
    merged = cat_agg.merge(totals, on=["County","FamilyStructure","Type"], how="left")
    merged["pct"] = (100 * merged["Cost"] / merged["Total"]).round(0)
    order = {v:i for i,v in enumerate(COST_VARIABLES)}
    merged["sort_idx"] = merged["CostVariable"].map(order)
    merged = merged.sort_values(["County","FamilyStructure","Type","sort_idx"])
    def mk_text(g):
        return "<br>".join(f"{r.CostVariable}: ${int(round(r.Cost)):,} ({int(r.pct)}%)" for r in g.itertuples())
    breakdown = (merged
             .groupby(["County","FamilyStructure","Type"])
             .apply(mk_text)
             .reset_index(name="breakdown"))
    totals = totals.merge(breakdown, on=["County","FamilyStructure","Type"], how="left")
    return totals, cat_agg

# ----- load GeoJSON (no GeoPandas) -----
with open(GEOJSON_FILE, "r") as f:
    geojson_dict = json.load(f)

props0 = geojson_dict["features"][0]["properties"]
# detect county-name property (first key containing "name")
candidates = [k for k in props0.keys() if re.search(r"name", k, re.I)]
NAME_PROP = candidates[0] if candidates else list(props0.keys())[0]

# full county list from geojson
COUNTY_LIST = [str(feat["properties"].get(NAME_PROP, "")).strip() for feat in geojson_dict["features"]]

# ----- load CSVs + compute totals -----
long_min = build_long_for_type("min", CSV_FILES["min"])
long_avg = build_long_for_type("avg", CSV_FILES["avg"])
df_long = add_miscellaneous(pd.concat([long_min, long_avg], ignore_index=True))
totals, _ = build_totals_and_breakdown(df_long)

def make_fig(fs_value, type_value):
    # subset totals
    sub = totals[(totals["FamilyStructure"]==fs_value) & (totals["Type"]==type_value)].copy()
    # base frame of all counties
    base = pd.DataFrame({NAME_PROP: COUNTY_LIST})
    # align CSV county name to geojson county name
    sub = sub.rename(columns={"County": NAME_PROP})
    m = base.merge(sub[[NAME_PROP,"Total","breakdown"]], on=NAME_PROP, how="left")

    color_scale = "Plasma" if type_value=="min" else "Viridis"
    fig = px.choropleth_mapbox(
        m, geojson=geojson_dict, locations=NAME_PROP, featureidkey=f"properties.{NAME_PROP}",
        color="Total", color_continuous_scale=color_scale,
        hover_name=NAME_PROP,
        hover_data={"Total":":.0f","breakdown":True, NAME_PROP:False},
        mapbox_style="open-street-map",
        center={"lat":37.5,"lon":-79.0}, zoom=6.6, opacity=0.85
    )
    fig.update_layout(
        title=f"Total Monthly Cost — {'Minimum' if type_value=='min' else 'Average'}<br><sup>{fs_value}</sup>",
        paper_bgcolor="white", plot_bgcolor="white", template="plotly_white",
        margin=dict(t=60,l=0,r=0,b=0),
        coloraxis_colorbar_title="USD"
    )
    fig.update_traces(hovertemplate="<b>%{hovertext}</b><br><br>Total: $%{customdata[0]:,.0f}<br>%{customdata[1]}<extra></extra>")
    return fig

# ----- Dash app -----
app = dash.Dash(__name__, title="VA Cost of Living", suppress_callback_exceptions=True)
app.layout = html.Div(style={"background":"white","minHeight":"100vh","padding":"12px"}, children=[
    html.H2("Virginia Cost of Living — Interactive Maps (Python/Dash)", style={"margin":"0 0 8px 0"}),
    html.Div("White background • CSV/GeoJSON loaded from CoL_data • Tabs + Dropdown",
             style={"color":"#666","marginBottom":"12px"}),

    html.Div([
        html.Label("Family structure", style={"fontWeight":"600"}),
        dcc.Dropdown(id="fs-dd",
                     options=[{"label":fs, "value":fs} for fs in FAMILY_STRUCTURES],
                     value="2 Adults + 2 Children", clearable=False,
                     style={"maxWidth":"480px"})],
        style={"marginBottom":"8px"}),

    dcc.Tabs(id="tabs", value="min", children=[
        dcc.Tab(label="Minimum Cost", value="min"),
        dcc.Tab(label="Average Cost", value="avg"),
    ], colors={"border":"#e6e6e6","primary":"#000","background":"#fff"}),

    dcc.Graph(id="map-fig", style={"height":"80vh","background":"white"})
])

@app.callback(Output("map-fig","figure"), Input("fs-dd","value"), Input("tabs","value"))
def update_map(fs_value, type_value):
    return make_fig(fs_value, type_value)

if __name__ == "__main__":
    app.run(debug=True)
