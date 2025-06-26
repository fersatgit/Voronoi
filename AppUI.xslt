<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:frmwrk="Corel Framework Data">
	<xsl:template match="node()|@*">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
		</xsl:copy>
	</xsl:template>
	<xsl:template match="uiConfig/items">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
			<itemData guid="Voronoi1" type="button" userCaption="Диаграмма Вороного" noBmpOnMenu="true" onInvoke="*Bind(DataSource=Voronoi;Path=0)" enable="*Cql(SelectionInfoDatasource.IsValidCombine or (SelectionInfoDatasource.NumSelected==1 and SelectionInfoDatasource.AnySelectedIsCurves))"/>
			<itemData guid="Voronoi2" type="spinner" appearance="tight" minorTickFreq="1" rangeMin="6" rangeMax="999" increment="1" length="70" value="*Bind(DataSource=Voronoi;Path=1;BindType=TwoWay)"/>
			<itemData guid="Voronoi3" type="spinner" appearance="tight" decimalPlaces="1" showUnit="true" internalUnit="2" displayUnit="2" minorTickFreq="0.1" rangeMin="0" rangeMax="99" increment="0.1" length="70" value="*MulF(*Bind(DataSource=Voronoi;Path=2;BindType=TwoWay),2)"/>
			<itemData guid="Voronoi4" type="spinner" appearance="tight" decimalPlaces="1" internalUnit="2" displayUnit="2" showUnit="true" minorTickFreq="0.1" rangeMin="0" rangeMax="99" increment="0.1" length="70" value="*Bind(DataSource=Voronoi;Path=3;BindType=TwoWay)"/>
			<itemData guid="Voronoi5" type="button" appearance="dialog" userCaption="Ok" sizeType="comboSize" onInvoke="*Bind(DataSource=Voronoi;Path=4)"/>
			<itemData guid="Voronoi6" type="statusText" justification="right" text="Количество точек&#13;&#13;Зазор&#13;&#13;Радиус скругления" multiLine="true" appearance="ControlHeight"/>
		</xsl:copy>
	</xsl:template>
	<xsl:template match="uiConfig/dialogs">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
			<dialog guid="Voronoi" defaultPushButton="Voronoi5" initialFocus="Voronoi5">
				<container>
					<item dock="left" guidRef="Voronoi6"/>
					<container dock="right">
						<item guidRef="Voronoi2"/>
						<item guidRef="Voronoi3"/>
						<item guidRef="Voronoi4"/>
					</container>
					<item dock="bottom" guidRef="crlfrmwk_spacer_mini"/>
					<item dock="bottom" guidRef="Voronoi5"/>
				</container>
			</dialog>
		</xsl:copy>
	</xsl:template>
	<xsl:template match="uiConfig/commandBars/commandBarData[@guid='21cd018a-87c1-476c-9a98-a8e36b9c479e']/menu/item[@guidRef='1c86e69f-30a5-4a01-85cb-e4a76cca5fd5']">
		<xsl:copy-of select="."/>
		<item guidRef="Voronoi1"/>
	</xsl:template>
</xsl:stylesheet>