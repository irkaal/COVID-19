import { Component, OnInit } from '@angular/core';
import * as L from 'leaflet';

import { DataService } from '../data.service';

@Component({
  selector: 'app-map',
  templateUrl: './map.component.html',
  styleUrls: ['./map.component.scss'],
})
export class MapComponent implements OnInit {
  private _isDarkLayer!: boolean;
  private _layer!: L.Layer;
  private _map!: L.Map;
  private _mapData!: {
    [Country_Region: string]: {
      [Field: string]: string[];
    };
  };

  constructor(private _dataService: DataService) {}

  ngOnInit(): void {
    this.initMap();
    this._dataService.getData().then((data) => {
      this._mapData = data;
      this.addMarkers();
    });
  }

  public updateTile(): void {
    this._isDarkLayer = !this._isDarkLayer;
    if (this._layer) this._map.removeLayer(this._layer);
    this._layer = L.tileLayer(
      `https://tiles.stadiamaps.com/tiles/${
        this._isDarkLayer ? 'alidade_smooth_dark' : 'alidade_smooth'
      }/{z}/{x}/{y}{r}.png`,
      {
        maxZoom: 20,
        attribution: `
          &copy; <a href="https://stadiamaps.com/">Stadia Maps</a>,
          &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a>
          &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors`,
      }
    ).addTo(this._map);
  }

  private initMap(): void {
    this._map = L.map('map', {
      center: [30, 0],
      zoom: 3,
    });
    this.updateTile();
  }

  private addMarkers(): void {
    console.log('Adding markers...');

    const countryRegionList = Object.keys(this._mapData).filter(
      (value: string) => value !== 'Worldwide'
    );

    countryRegionList.forEach((countryRegion: string) => {
      const crData = this._mapData[countryRegion];
      const lat = Number(crData.Latitude[crData.Latitude.length - 1]);
      const lon = Number(crData.Longitude[crData.Longitude.length - 1]);
      const confirmed = Number(crData.Confirmed[crData.Confirmed.length - 1]);
      L.circleMarker([lat, lon], {
        radius: Math.sqrt(Math.sqrt(confirmed)),
        weight: 0,
        fillOpacity: 0.3,
        color: 'rgb(255, 64, 129)',
      }).addTo(this._map);
    });
  }
}
